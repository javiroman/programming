package controllers

import (
	"context"
	"fmt"
	javiromanv1alpha1 "github/javiroman/operators/visitors-operator/api/v1alpha1"
	appsv1 "k8s.io/api/apps/v1"
	"k8s.io/apimachinery/pkg/api/errors"
	"k8s.io/apimachinery/pkg/runtime"
	ctrl "sigs.k8s.io/controller-runtime"
	"sigs.k8s.io/controller-runtime/pkg/client"
	ctrllog "sigs.k8s.io/controller-runtime/pkg/log"
	"time"
)

var log = ctrllog.Log.WithName("controller_visitorapp")

// VisitorAppReconciler reconciles a VisitorApp object
type VisitorAppReconciler struct {
	client.Client
	Scheme *runtime.Scheme
}

//+kubebuilder:rbac:groups=javiroman.javiroman.lan,resources=visitorapps,verbs=get;list;watch;create;update;patch;delete
//+kubebuilder:rbac:groups=javiroman.javiroman.lan,resources=visitorapps/status,verbs=get;update;patch
//+kubebuilder:rbac:groups=javiroman.javiroman.lan,resources=visitorapps/finalizers,verbs=update

// Reconcile is part of the main kubernetes reconciliation loop which aims to
// move the current state of the cluster closer to the desired state.
// TODO(user): Modify the Reconcile function to compare the state specified by
// the VisitorApp object against the actual cluster state, and then
// perform operations to make the cluster state reflect the state specified by
// the user.
//
// For more details, check Reconcile and its Result here:
// - https://pkg.go.dev/sigs.k8s.io/controller-runtime@v0.11.0/pkg/reconcile
func (r *VisitorAppReconciler) Reconcile(ctx context.Context, req ctrl.Request) (ctrl.Result, error) {
	log := ctrllog.FromContext(ctx)

	log.Info("Reconciling VisitorsApp", "Request.Namespace", req.Namespace, "Request.Name", req.Name)

	// Fetch the VisitorsApp instance
	v := &javiromanv1alpha1.VisitorApp{}
	err := r.Client.Get(context.TODO(), req.NamespacedName, v)
	if err != nil {
		if errors.IsNotFound(err) {
			// Request object not found, could have been deleted after ctrl req.
			// Owned objects are automatically garbage collected. For additional cleanup logic use finalizers.
			// Return and don't requeue
			return ctrl.Result{}, nil
		}
		// Error reading the object - requeue the req.
		return ctrl.Result{}, err
	}

	var result *ctrl.Result

	// == MySQL ==========
	result, err = r.ensureSecret(req, v, r.mysqlAuthSecret(v))
	if result != nil {
		return *result, err
	}

	result, err = r.ensureDeployment(req, v, r.mysqlDeployment(v))
	if result != nil {
		return *result, err
	}

	result, err = r.ensureService(req, v, r.mysqlService(v))
	if result != nil {
		return *result, err
	}

	mysqlRunning := r.isMysqlUp(v)

	if !mysqlRunning {
		// If MySQL isn't running yet, requeue the reconcile
		// to run again after a delay
		delay := time.Second * time.Duration(5)

		log.Info(fmt.Sprintf("MySQL isn't running, waiting for %s", delay))
		return ctrl.Result{RequeueAfter: delay}, nil
	}

	// == Visitors Backend  ==========
	result, err = r.ensureDeployment(req, v, r.backendDeployment(v))
	if result != nil {
		return *result, err
	}

	result, err = r.ensureService(req, v, r.backendService(v))
	if result != nil {
		return *result, err
	}

	err = r.updateBackendStatus(v)
	if err != nil {
		// Requeue the req if the status could not be updated
		return ctrl.Result{}, err
	}

	result, err = r.handleBackendChanges(v)
	if result != nil {
		return *result, err
	}

	// == Visitors Frontend ==========
	result, err = r.ensureDeployment(req, v, r.frontendDeployment(v))
	if result != nil {
		return *result, err
	}

	result, err = r.ensureService(req, v, r.frontendService(v))
	if result != nil {
		return *result, err
	}

	err = r.updateFrontendStatus(v)
	if err != nil {
		// Requeue the req
		return ctrl.Result{}, err
	}

	result, err = r.handleFrontendChanges(v)
	if result != nil {
		return *result, err
	}

	// == Finish ==========
	// Everything went fine, don't requeue

	return ctrl.Result{}, nil
}

// SetupWithManager sets up the controller with the Manager.
func (r *VisitorAppReconciler) SetupWithManager(mgr ctrl.Manager) error {
	return ctrl.NewControllerManagedBy(mgr).
		For(&javiromanv1alpha1.VisitorApp{}).
		Owns(&appsv1.Deployment{}).
		Complete(r)
}
