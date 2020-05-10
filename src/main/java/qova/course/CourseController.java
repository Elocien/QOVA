package qova.course;

import java.util.Base64;
import java.util.Objects;

import java.util.Optional;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import qova.responses.ResponseRepository;



@Controller // This means that this class is a Controller
public class CourseController {


    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final CourseRepository courseRepository;

    @Autowired
    private final ResponseRepository responseRepository;

    @Autowired
    CourseController(CourseManagement courseManagement, CourseRepository courseRepository, ResponseRepository responseRepository) {
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.courseRepository = Objects.requireNonNull(courseRepository);
        this.responseRepository = Objects.requireNonNull(responseRepository);

    }

    @GetMapping("/")
    public String welcome () {
        return "home";
    }


    //Shows a table containing all courses 
    @GetMapping("/courses")
    public String courses (Model model) {

        model.addAttribute("courseList", courseRepository.findAll());
        return "courses";
    }


    //Shows the details for a specific course
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


            //QRCode URL (Redirects to a courses survey when scanned)
            String LectureSurveyURl = "localhost:8080/survey?type=LECTURE&id="+ course.get().getId();
            String TutorialSurveyURl = "localhost:8080/survey?type=TUTORIAL&id="+ course.get().getId();
            String SeminarSurveyURl = "localhost:8080/survey?type=SEMINAR&id="+ course.get().getId();


            //send byte array (the QRCode image) to model 
            model.addAttribute("LectureQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(LectureSurveyURl)));  //TODO: course.get().getName() needs to be replaced with URL String
            model.addAttribute("TutorialQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(TutorialSurveyURl)));  //TODO: course.get().getName() needs to be replaced with URL String
            model.addAttribute("SeminarQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(SeminarSurveyURl)));  //TODO: course.get().getName() needs to be replaced with URL String
            
            return "courseDetails";
        } else {
			return "redirect:../courses";
		}
    }

        




    //Create Course
    @GetMapping("course/create")
	public String createCourse(Model model, CourseForm form) {

        model.addAttribute("form", form);

        //List of Semesters for Course Creator to pick from
        model.addAttribute("semesterDates", courseManagement.findSemesters());
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
            model.addAttribute("semesterDates", courseManagement.findSemesters());
			model.addAttribute("course", crs.get());
			return "courseEdit";
		} else {
			return "redirect:../courses";
		}
    }


    
    //Edit Course Validation (when course is updated, check wether the fields are all appropriately set e.g. NotNull)
    @PostMapping("/course/edit")
	public String editCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
			BindingResult result, @RequestParam String id) {

		if (result.hasErrors()) {
			return editCourse(model, form, id);
		}

		courseManagement.updateCourseDetails(id, form);
		return "redirect:../courses";
	}











    //Call Questioneditor and Submit created Survey
    //---------------------------------------------------------------------------




    //Mapping for surveyeditor HTML (called from CourseDetails Page!)
    @GetMapping("/course/surveyeditor")
    public String questioneditor(Model model, @RequestParam String type, @RequestParam(required = false) String id){
        model.addAttribute("typeID", type);
        model.addAttribute("id", id);
        model.addAttribute("survey", courseManagement.getSurveyforType(id, type));
        return "questioneditor4";
    }


    //Mapping to submit a questionaire 
    @PostMapping("/course/surveyeditor")
    public String questioneditorSubmit(SurveyForm form, @RequestParam String type, @RequestParam(required = false) String id) {
        

        //Form empty -> Redirect to details again 
        if (form.getQuestionnairejson().length()==0) {
            return "redirect:../course/details" + "?id=" + id;          //TODO: Redirects back course at the moment, think about where this should go
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseRepository.findById(id);
        if (course.isPresent()){


            // if type is none of the correct values
            if((type != "LECTURE") && (type != "TUTORIAL") && (type != "SEMINAR")){
                //TODO: Where to go from here? Back to Survey or error html
                System.out.println("type is null");
            }

            else{
                //Method from courseManager which sets the survey for the relevant surveyType
                courseManagement.setSurveyforType(id, type, form);
            }


            //Redirect back to CourseDetails page
            return "redirect:../course/details" + "?id=" + id;
        }
        else{
            return "redirect:../courses";
        }
    }
    

    //---------------------------------------------------------------------------









    //Get Survey from Server 
    //---------------------------------------------------------------------------

    //Mapping for Survey html view
    @GetMapping("/survey")
    public String SuveyView (Model model, @RequestParam String type, @RequestParam(required = false) String id){
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


    //Mapping to recieve SURVEY from server
    @GetMapping("/survey/get")
    @ResponseBody
    public String sendSurvey( @RequestParam String type, @RequestParam(required = false) String id){
        
        //redirect 
        if (id == null) {
			return null;
        }
        
        else{
            return courseManagement.getSurveyforType(id, type);
        }
    }

    //---------------------------------------------------------------------------
   








    //PostMapping to submit survey and serialize results
    //---------------------------------------------------------------------------
    @PostMapping("survey")
    public ResponseEntity recieveResponseJSON(SurveyForm form, @RequestParam String type, @RequestParam(required = false) String id){
        
        //get JSON Response as string
        String JsonResponse = form.getQuestionnairejson();
        

        //Deserialize the String to a JavaObject Response (package qova.responses)
        // response = Deserialize(JsonResponse);

        //Save object 
        // responseRepository.save(response)
        
        //if all goes well
        return ResponseEntity.ok(HttpStatus.OK);
    }

    //---------------------------------------------------------------------------





    //test method
    @GetMapping("/create")
    public String createTest(){
        courseManagement.TestCreateCourse();
        return "home";
    }


}


