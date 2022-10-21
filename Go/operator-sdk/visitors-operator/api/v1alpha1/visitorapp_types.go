/*
Copyright 2022.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package v1alpha1

import (
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
)

// EDIT THIS FILE!  THIS IS SCAFFOLDING FOR YOU TO OWN!
// NOTE: json tags are required.  Any new fields you add must have json tags for the fields to be serialized.

// VisitorAppSpec defines the desired state of VisitorApp
type VisitorAppSpec struct {
	// INSERT ADDITIONAL SPEC FIELDS - desired state of cluster
	// Important: Run "make" to regenerate code after modifying this file

	// Foo is an example field of VisitorApp. Edit visitorapp_types.go to remove/update
	Size  int32  `json:"size"`
	Title string `json:"title"`
}

// VisitorAppStatus defines the observed state of VisitorApp
type VisitorAppStatus struct {
	// INSERT ADDITIONAL STATUS FIELD - define observed state of cluster
	// Important: Run "make" to regenerate code after modifying this file
	BackendImage  string `json:"backendImage"`
	FrontendImage string `json:"frontendImage"`
}

//+kubebuilder:object:root=true
//+kubebuilder:subresource:status

// VisitorApp is the Schema for the visitorapps API
type VisitorApp struct {
	metav1.TypeMeta   `json:",inline"`
	metav1.ObjectMeta `json:"metadata,omitempty"`

	Spec   VisitorAppSpec   `json:"spec,omitempty"`
	Status VisitorAppStatus `json:"status,omitempty"`
}

//+kubebuilder:object:root=true

// VisitorAppList contains a list of VisitorApp
type VisitorAppList struct {
	metav1.TypeMeta `json:",inline"`
	metav1.ListMeta `json:"metadata,omitempty"`
	Items           []VisitorApp `json:"items"`
}

func init() {
	SchemeBuilder.Register(&VisitorApp{}, &VisitorAppList{})
}
