package qova.course;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import qova.survey.Survey;
import qova.survey.SurveyManagement;
import qova.survey.SurveyRepository;




@Controller // This means that this class is a Controller
@RequestMapping(path="/demo") // This means URL's start with /demo (after Application path)
public class CourseController {
  @Autowired // This means to get the bean called userRepository
         // Which is auto-generated by Spring, we will use it to handle the data
  private SurveyRepository surveyRepository;

  @PostMapping(path="/add") // Map ONLY POST Requests
  public @ResponseBody String addNewSurvey (@RequestParam String questions) {
    // @ResponseBody means the returned String is the response, not a view name
    // @RequestParam means it is a parameter from the GET or POST request

    Survey n = new Survey();
    n.setQuestions(questions);
    surveyRepository.save(n);
    return "questioneditor2";
  }

  @GetMapping(path="/all")
  public @ResponseBody Iterable<Survey> getAllSurveys() {
    // This returns a JSON or XML with the users
    return surveyRepository.findAll();
  }
}









// @Controller
// public class CourseController {

//     private final CourseManagement courseManagement;
//     private final SurveyManagement surveyManagement;

//     @Autowired
// 	CourseController(CourseManagement courseManagement, SurveyManagement surveyManagement) {
//         this.courseManagement = Objects.requireNonNull(courseManagement);
//         this.surveyManagement = surveyManagement;
// 	}


//     @GetMapping("/")
//     public String welcome (){
//         surveyManagement.createSurvey();
//         return "home";
//     }


//     @GetMapping("/1")
//     public String welcome2 () {return "questioneditor";}

//     @GetMapping("/2")
//     public String welcome3 () {return "questioneditor2";}





// }

