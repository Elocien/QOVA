package qova.course;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

import qova.survey.SurveyManagement;

@Controller
public class CourseController {

    private final CourseManagement courseManagement;
    private final SurveyManagement surveyManagement;

    @Autowired
	CourseController(CourseManagement courseManagement, SurveyManagement surveyManagement) {
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.surveyManagement = surveyManagement;
	}


    @GetMapping("/")
    public String welcome (){
        // surveyManagement.createSurvey();
        return "home";
    }


    @GetMapping("/1")
    public String welcome2 () {return "questioneditor";}

    @GetMapping("/2")
    public String welcome3 () {return "questioneditor2";}
}