package qova.admin;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import org.springframework.web.bind.annotation.ResponseBody;
import qova.enums.CourseType;
import qova.forms.SurveyForm;
import qova.logic.CourseManagement;
import qova.logic.ResponseManagement;
import qova.objects.Course;
import qova.users.CurrentUserDetails;
import qova.users.User;
import qova.users.UserManagement;


@Controller
public class AdminController {
    
    private final AdminManagement adminManagement;

    private final ResponseManagement responseManagement;

    private final CourseManagement courseManagement;

    private final UserManagement userManagement;

    AdminController(AdminManagement adminManagement, ResponseManagement responseManagement,
                    CourseManagement courseManagement, UserManagement userManagement){
        this.adminManagement = Objects.requireNonNull(adminManagement);
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.userManagement = Objects.requireNonNull(userManagement);
    }


    

    //Default-Survey methods
    //---------------------------------------------------------------------------

    @PreAuthorize("hasAnyRole('ADMIN')")
    @GetMapping("/admin/home")
    public String adminLogin() {
        return "adminPanel";
    }


    @PreAuthorize("hasAnyRole('ADMIN')")
    @GetMapping("/admin/updateDefaultSurvey")
    public String updateDefaultSurvey(Model model, @RequestParam String type){

        CourseType courseType = responseManagement.parseCourseType(type);

        //give previous default survey to model
        model.addAttribute("survey", adminManagement.getDefaultSurvey(courseType));
        model.addAttribute("type", courseType);

        return "adminQuestioneditor";
    }



    //Mapping to submit a questionaire
    @PreAuthorize("hasAnyRole('ADMIN')")
    @PostMapping("/admin/updateDefaultSurvey")
    public String defaultSurveySubmit(SurveyForm form, @RequestParam String type){

        adminManagement.updateDefaultSurvey(form, responseManagement.parseCourseType(type));

        return "adminPanel";
    }

    @GetMapping("/delete")
    public String deleteCourse(@RequestParam UUID id, @AuthenticationPrincipal CurrentUserDetails userDetails) {
        if(userDetails.getUsername().equals("https://idp.tu-dresden.de/idp/shibboleth!https://qova.med.tu-dresden.de/shibboleth!YA5MO4SfcGmbXRgccVo6IMWfX0k=")){
            courseManagement.deleteCourse(id);
            return "home";
        }
        return "redirect:../course/list";
    }

    @GetMapping("/course/username")
    public String getUsername(Model model, @AuthenticationPrincipal UserDetails userDetails){
        model.addAttribute("username", userDetails.getUsername());

        return "usernameDisplayPage";
    }

    @ResponseBody
    @GetMapping("/course/userCourses")
    public String getUserCourses(Model model, @AuthenticationPrincipal UserDetails userDetails, String userId){
        if(userDetails.getUsername().equals("https://idp.tu-dresden.de/idp/shibboleth!https://qova.med.tu-dresden.de/shibboleth!YA5MO4SfcGmbXRgccVo6IMWfX0k=")){
            String courseOwnerList = "";
            for(Course crs : courseManagement.findByOwnerid(userId)){
                courseOwnerList = courseOwnerList + "Course name: " + crs.getName() + " - Owner Username: " + crs.getOwnerId() + "\n\n";
            }

            return courseOwnerList;
        }

        return "None found";
    }

    @ResponseBody
    @GetMapping("/course/courseOwner")
    public String getCourseOwner(Model model, @AuthenticationPrincipal UserDetails userDetails, @RequestParam UUID courseId){
        if(userDetails.getUsername().equals("https://idp.tu-dresden.de/idp/shibboleth!https://qova.med.tu-dresden.de/shibboleth!YA5MO4SfcGmbXRgccVo6IMWfX0k=")) {
            return courseManagement.findById(courseId).get().getOwnerId();
        }
        return "None found";
    }
    
    @ResponseBody
    @GetMapping("/admin/management")
    public String adminUserManagement(){
        Optional<User> usr = userManagement.findById("https://idp.tu-dresden.de/idp/shibboleth!https://qova.med.tu-dresden.de/shibboleth!YA5MO4SfcGmbXRgccVo6IMWfX0k=");
        StringBuilder string = new StringBuilder();
        string.append("UserRole: " + usr.get().getUserRole());
        Boolean result = userManagement.setAdminUser("https://idp.tu-dresden.de/idp/shibboleth!https://qova.med.tu-dresden.de/shibboleth!YA5MO4SfcGmbXRgccVo6IMWfX0k=");
        string.append("\n Result: " + result);
        string.append("\n NewUserRole: " + usr.get().getUserRole());
        string.append(userManagement.findAdminUsers().toString());
        return string.toString();
    }
    
}