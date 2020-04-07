package qova.course;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import qova.survey.Survey;
import qova.survey.SurveyManagement;
import qova.survey.SurveyRepository;



@Controller // This means that this class is a Controller
public class CourseController {

    @Autowired
    private final SurveyRepository surveyRepository;

    @Autowired
    private final SurveyManagement surveyManagement;

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final CourseRepository courseRepository;

    @Autowired
    CourseController(CourseManagement courseManagement, CourseRepository courseRepository, SurveyManagement surveyManagement, SurveyRepository surveyRepository) {
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.courseRepository = Objects.requireNonNull(courseRepository);
        this.surveyManagement = Objects.requireNonNull(surveyManagement);
        this.surveyRepository = Objects.requireNonNull(surveyRepository);

    }

    @PostMapping(path="/add") // Map ONLY POST Requests
    public @ResponseBody String addNewSurvey (@RequestParam String questions) {
        // @ResponseBody means the returned String is the response, not a view name
        // @RequestParam means it is a parameter from the GET or POST request

        Survey n = new Survey();
        n.setQuestions(questions);
        surveyRepository.save(n);
        return "Saved";
    }

    @GetMapping(path="/all")
    public @ResponseBody Iterable<Survey> getAllSurveys() {
        // This returns a JSON or XML with the users
        return surveyRepository.findAll();
    }

    @GetMapping("/1")
    public String welcome2 () {
        surveyManagement.createSurvey();
        return "questioneditor";
    }

    @GetMapping("/2")
    public String welcome3 () {
        surveyManagement.deleteSurvey();
        return "questioneditor2";
    }
}



