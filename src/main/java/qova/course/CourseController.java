package qova.course;

import java.util.Objects;

import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
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

    @GetMapping("/")
    public String welcome () {
        courseManagement.createCourse();
        return "home";
    }

    @GetMapping("/courses")
    public String courses (Model model) {
        model.addAttribute("courseList", courseRepository.findAll());
        return "courses";
    }

    @GetMapping("/course/details")
    public String courseDetails(Model model, @RequestParam(required = false) Long id) throws Exception{
        
        //redirect 
        if (id == null) {
			return "redirect:../courses";
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);
        if (course.isPresent()){
            model.addAttribute("course", course.get());
            model.addAttribute("qrcode", courseManagement.generateQRCode(course.get()));
            return "courseDetails";
        } else {
			return "redirect:../courses";
		}
    }

    @PostMapping("/courses/delete")
	public String courseDelete(@RequestParam Long id) {
		courseManagement.deleteCourse(id);
		return "redirect:../courses";
	}

    @GetMapping("/1")
    public String welcome2 () {
        return "questioneditor";
    }

    @GetMapping("/2")
    public String welcome3 () {
        return "questioneditor2";
    }

    @GetMapping("/3")
    public String welcome4(){
        return "questioneditor3";
    }
}


