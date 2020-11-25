package qova.admin;

import java.util.Objects;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.enums.CourseType;
import qova.forms.SurveyForm;
import qova.logic.ResponseManagement;



@Controller
public class AdminController {
    
    private final AdminManagement adminManagement;

    private final ResponseManagement responseManagement;

    AdminController(AdminManagement adminManagement, ResponseManagement responseManagement){
        this.adminManagement = Objects.requireNonNull(adminManagement);
        this.responseManagement = Objects.requireNonNull(responseManagement);
    }


    

    //Default-Survey methods
    //---------------------------------------------------------------------------

    @GetMapping("/admin/home")
    public String adminLogin() {
        return "adminPanel";
    }


    @GetMapping("/admin/updateDefaultSurvey")
    public String updateDefaultSurvey(Model model, @RequestParam String type){

        CourseType courseType = responseManagement.parseCourseType(type);

        //give previous default survey to model
        model.addAttribute("survey", adminManagement.getDefaultSurvey(courseType));
        model.addAttribute("type", courseType);

        return "adminQuestioneditor";
    }



    //Mapping to submit a questionaire 
    @PostMapping("/admin/updateDefaultSurvey")
    public String defaultSurveySubmit(SurveyForm form, @RequestParam String type){

        adminManagement.updateDefaultSurvey(form, responseManagement.parseCourseType(type));

        return "adminPanel";
    }
    

    
}