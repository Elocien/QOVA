package qova.forms;

import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.ui.Model;

import java.util.UUID;

/**
 * Form used in {@linkplain qova.admin.AdminController#setCourseOwner(Model, UserDetails, UUID, CourseOwnerForm)}
 * to set the courseOwner for a {@linkplain qova.objects.Course}
 */
public class CourseOwnerForm {
    /**
     * The string containing the userId for the new Owner
     */
    private String userId;

    /**
     * Constructor
     * @param userId containing the userId string
     */
    public CourseOwnerForm(String userId){
        this.userId = userId;
    }

    /**
     * Getter
     * @return the userId of the owner to be set
     */
    public String getCourseOwner() {
        return this.userId;
    }
}
