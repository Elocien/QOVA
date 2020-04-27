package qova.course;

import java.util.Base64;
import java.util.Objects;

import java.util.Optional;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;



@Controller // This means that this class is a Controller
public class CourseController {


    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final CourseRepository courseRepository;

    @Autowired
    CourseController(CourseManagement courseManagement, CourseRepository courseRepository) {
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.courseRepository = Objects.requireNonNull(courseRepository);

    }

    @GetMapping("/")
    public String welcome () {
        return "home";
    }

    @GetMapping("/courses")
    public String courses (Model model) {
        model.addAttribute("courseList", courseRepository.findAll());
        return "courses";
    }

    @GetMapping("/course/details")
    public String courseDetails(Model model, @RequestParam(required = false) String id) throws Exception {
        
        //redirect 
        if (id == null) {
			return "redirect:../courses";
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);
        if (course.isPresent()){
            model.addAttribute("course", course.get());


            //send byte array to model
            model.addAttribute("image", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage((course.get().getName()))));
            
            return "courseDetails";
        } else {
			return "redirect:../courses";
		}
    }

        
    @GetMapping("course/create")
	public String createCourse(Model model, CourseForm form) {

		model.addAttribute("form", form);
		return "courseCreate";
	}

	@PostMapping("course/create")
	public String createCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
			BindingResult result) {


		if (result.hasErrors()) {
			return createCourse(model, form);
		}

		courseManagement.createCourse(form);
		return "redirect:../courses";
	}


    //Mapping for Survey html view
    @GetMapping("course/survey")
    public String SuveyView (@RequestParam(required = false) String id){
        //redirect 
        if (id == null) {
			return "redirect:../";
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);
        if (course.isPresent()){
            return "survey";
        }else{
            return "redirect:../";
        }
    }


    //Mapping for getting JSON from server to Javascript
    @GetMapping("course/survey/get")
    @ResponseBody
    public String[] sendSurvey(@RequestParam(required = false) String id){
        
        //redirect 
        if (id == null) {
			return null;
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);
        if (course.isPresent()){
            Course crs = course.get();

            //Serialize before sending?
            return crs.getSurvey();

        }else{
            return null;
        }
    }




    // //Mapping for submitting the Suvey and saving it to database
    // @PostMapping("survey")
    // public void saveSurvey(@RequestParam(required = false) String id){
    //     //Deserialization to Java Object using Response
    // }

    
    


    
    
    @PostMapping("/courses/delete")
	public String courseDelete(@RequestParam String id) {
		courseManagement.deleteCourse(id);
		return "redirect:../courses";
	}

    

    @GetMapping("/course/editor")
    public String fragebogeneditor(@RequestParam(required = false) String id){
        return "questioneditor4";
    }

    @PostMapping("/survey")
    public String welcome6(Form form) {
        if (form.getQuestionnairejson().length()>0) {

            return "redirect:/";
        }
        else   {
            return "questioneditor4";
        }
    }





    //test method
    @GetMapping("/create")
    public String createTest(){
        courseManagement.TestCreateCourse();
        return "home";
    }
}


