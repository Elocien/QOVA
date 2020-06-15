package qova.admin;

import java.util.Optional;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.course.SurveyForm;



@Controller
public class AdminController {
    

    AdminController(){
    }


    

    //Default-Survey methods
    //---------------------------------------------------------------------------

    @GetMapping("/admin")
    public String adminLogin(Model model){

        model.addAttribute("defaultSurvey", );

        return "questioneditor";
    }



    //Mapping for surveyeditor HTML (called from CourseDetails Page!)
    @GetMapping("/course/surveyeditor")
    public String questioneditor(Model model, @RequestParam String type, @RequestParam(required = false) String id){
        
        //Give model the following attributes, which are used to submit the survey, via the post method
        model.addAttribute("typeID", type);
        model.addAttribute("id", id);

        //Gives the survey JSON to the model, so the current survey can be assembled and added to
        model.addAttribute("survey", courseManagement.getSurveyforType(id, type));

        //give course name to model, to show as title
        Optional<Course> course = courseManagement.findById(id);
        model.addAttribute("coursename", course.get().getName());
        
        /*
        //Just for testing:
        LocalDate dateNow = LocalDate.now();
        var course = new Course("Cheese", true, true, true, "Cheese Lecture Survey 2020", "Cheese Tutorial Survey 2020", "Cheese Seminar Survey 2020", 5, 5, 5, CourseFaculty.EDUCATION, "1-5", dateNow);
        model.addAttribute("coursename", course.getName());
        */
        
        return "questioneditor";
    }


    //Mapping to submit a questionaire 
    @PostMapping("/admin/")
    public String defaultSurveySubmit(SurveyForm form) {






   
        //Validate that the questionnaire matches criteria (TODO: ask for criteria)
        String JsonString = form.getQuestionnairejson();

        //Remove [] to parse JSON
        JsonString = JsonString.substring(1,JsonString.length()-1);


        //TODO: iterate through array and check length



        //example string
        // [{"type":"YesNo","question":""},{"type":"MultipleChoice","question":"","answers":["1","2","3","4","5"]},{"type":"DropDown","question":"","answers":["Answer","Answer","Answer"]}]






        
        //fetch course 
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()){

            // if type is none of the correct values, then redirect to homepage
            if(!(type.equals("LECTURE")) && !(type.equals("TUTORIAL")) && !(type.equals("SEMINAR"))){
                //TODO: Where to go from here? Maybe send exception to trigger popup?
                return "redirect:/";

            }

            else{
                //Method from courseManager which sets the survey for the relevant surveyType
                courseManagement.setSurveyforType(id, type, form);
            }


            //Redirect back to CourseDetails page
            return "redirect:../course/details" + "?id=" + id;
        }
        else{
            //TODO: need more feedback here for the user. Change this!
            return "redirect:../courses";
        }
    }
    

    
}