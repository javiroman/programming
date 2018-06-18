package playerservice;

import java.lang.annotation.*;
import com.google.inject.BindingAnnotation;

@Retention(RetentionPolicy.RUNTIME)
@BindingAnnotation
@Target(ElementType. LOCAL_VARIABLE)
public @interface Good {}
