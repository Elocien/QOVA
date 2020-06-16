package qova.admin;

import java.util.Objects;
import java.util.Optional;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.course.SurveyForm;



@Controller
public class AdminController {
    
    private final AdminManagement adminManagement;

    AdminController(AdminManagement adminManagement){
        this.adminManagement = Objects.requireNonNull(adminManagement);
    }


    

    //Default-Survey methods
    //---------------------------------------------------------------------------

    @GetMapping("/admin")
    public String adminLogin() {
        return "adminPanel";
    }


    @GetMapping("/admin/updateDefaultSurvey")
    public String updateDefaultSurvey(Model model) throws Exception {

        //give previous default survey to model
        model.addAttribute("defaultSurvey", adminManagement.getDefaultSurvey() );


        return "adminQuestioneditor";
    }



    //Mapping to submit a questionaire 
    @PostMapping("/admin/updateDefaultSurvey")
    public String defaultSurveySubmit(SurveyForm form) throws Exception {

        adminManagement.updateDefaultSurvey(form);

        return "adminPanel";
    }
    

    
}