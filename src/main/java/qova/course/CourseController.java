package qova.course;

import java.util.Base64;
import java.util.Objects;

import java.util.Optional;

import javax.validation.Valid;
import javax.validation.constraints.Null;

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
            model.addAttribute("image", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage((course.get().getName()))));  //TODO: course.get().getName() needs to be replaced with URL String
            
            return "courseDetails";
        } else {
			return "redirect:../courses";
		}
    }

        




    //Create Course
    @GetMapping("course/create")
	public String createCourse(Model model, CourseForm form) {

		model.addAttribute("form", form);
		return "courseCreate";
	}

    //Validation of Created course
	@PostMapping("course/create")
	public String createCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
			BindingResult result) {


		if (result.hasErrors()) {
			return createCourse(model, form);
		}

        //Management Method returns String of new Course
        String id = courseManagement.createCourseReturnId(form);
        
        //Redirect to SurveyEditor to start creating survey
		return "redirect:../course/details" + "?id=" + id;
    }
    
    

    //Delete Course 
    @PostMapping("/course/delete")
	public String delteCourse(@RequestParam String id) {
		courseManagement.deleteCourse(id);
		return "redirect:../courses";
    }
    

    //Edit Course
    @GetMapping("/course/edit")
	public String editCourse(Model model, CourseForm form, @RequestParam(required = false) String id) {

		if (id == null) {
			return "redirect:../courses";
		}

		Optional<Course> crs = courseRepository.findById(id);
		if (crs.isPresent()) {
			model.addAttribute("form", form);
			model.addAttribute("course", crs.get());
			return "courseEdit";
		} else {
			return "redirect:../courses";
		}
    }
    
    //Edit Course Validation
    @PostMapping("/course/edit")
	public String editCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
			BindingResult result, @RequestParam String id) {

		if (result.hasErrors()) {
			return editCourse(model, form, id);
		}

		courseManagement.updateCourseDetails(id, form);
		return "redirect:../courses";
	}











    //In Progress
    //---------------------------------------------------------------------------



    //Mapping for surveyeditor HTML for LECTURES
    @GetMapping("/course/surveyeditor")
    public String questioneditorLecture(@RequestParam(required = false) String id){
        return "questioneditor4";
    }


    //Mapping to submit LECTURE questionaire (called from CourseDetails Page!)
    @PostMapping("/course/lecture/surveyeditor")
    public String questioneditorLectureSubmit(Form form, @RequestParam(required = false) String id) {
        
        //Form empty -> Redirect to details again 
        if (form.getQuestionnairejson().length()>0) {
            return questioneditorLecture(id);           //TODO: Redirects back to Questioneditor if Survey is empty
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);
        if (course.isPresent()){
            Course crs = course.get();
            crs.setLectureSurvey(form.getQuestionnairejson());
            return "redirect:../course/details" + "?id=" + id;
        }
        else{
            return "redirect:../courses";
        }
    }
    
    
        //Mapping to submit TUTORIAL questionaire (called from CourseDetails Page!)
        @PostMapping("/course/tutorial/surveyeditor")
        public String questioneditorTutorialSubmit(Form form, @RequestParam(required = false) String id) {
            
            //Form empty -> Redirect to details again 
            if (form.getQuestionnairejson().length()>0) {
                return questioneditorLecture(id);           //TODO: Redirects back to Questioneditor if Survey is empty
            }
            
            //fetch course and go to details if present
            Optional<Course> course = courseRepository.findById(id);
            if (course.isPresent()){
                Course crs = course.get();
                crs.setTutorialSurvey(form.getQuestionnairejson());
                return "redirect:../course/details" + "?id=" + id;
            }
            else{
                return "redirect:../courses";
            }
        }
    
    
        //Mapping to submit TUTORIAL questionaire (called from CourseDetails Page!)
        @PostMapping("/course/seminar/surveyeditor")
        public String questioneditorSeminarSubmit(Form form, @RequestParam(required = false) String id) {
            
            //Form empty -> Redirect to details again 
            if (form.getQuestionnairejson().length()>0) {
                return questioneditorLecture(id);           //TODO: Redirects back to Questioneditor if Survey is empty
            }
            
            //fetch course and go to details if present
            Optional<Course> course = courseRepository.findById(id);
            if (course.isPresent()){
                Course crs = course.get();
                crs.setSeminarSurvey(form.getQuestionnairejson());
                return "redirect:../course/details" + "?id=" + id;
            }
            else{
                return "redirect:../courses";
            }
        }


    //---------------------------------------------------------------------------




    //Finished I think
    //---------------------------------------------------------------------------

    //Mapping for Survey html view
    @GetMapping("course/survey")
    public String SuveyView (@RequestParam(required = false) String id){
        //redirect 
        if (id == null) {
			return "redirect:../";
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);

        //Validate that course exists, and that the survey is not empty
        if (course.isPresent()){
            return "survey";
        }
        
        //If condition not met, redirect to home
        else{
            return "redirect:../";
        }
    }


    //Mapping to recieve LECTURE SURVEY from server
    @GetMapping("course/survey/lecture/get")
    @ResponseBody
    public String sendLectureSurvey(@RequestParam(required = false) String id){
        
        //redirect 
        if (id == null) {
			return null;
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);
        if (course.isPresent()){
            return course.get().getLectureSurvey();
        }else{
            return null;
        }
    }

    //Mapping to recieve TUTORIAL SURVEY from server
    @GetMapping("course/survey/tutorial/get")
    @ResponseBody
    public String sendTutorialSurvey(@RequestParam(required = false) String id){
        
        //redirect 
        if (id == null) {
			return null;
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);
        if (course.isPresent()){
            return course.get().getTutorialSurvey();

        }else{
            return null;
        }
    }

    //Mapping to recieve LECTURE SURVEY from server
        @GetMapping("course/survey/seminar/get")
        @ResponseBody
        public String sendSeminarSurvey(@RequestParam(required = false) String id){
            
            //redirect 
            if (id == null) {
                return null;
            }
            
            //fetch course and go to details if present
            Optional<Course> course = courseRepository.findById(id);
            if (course.isPresent()){
                return course.get().getSeminarSurvey();
            }else{
                return null;
            }
        }


    //---------------------------------------------------------------------------

    

    

   












    //test method
    @GetMapping("/create")
    public String createTest(){
        courseManagement.TestCreateCourse();
        return "home";
    }
}


