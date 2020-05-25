package qova.course;

import java.io.IOException;
import java.util.Base64;
import java.util.Objects;

import java.util.Optional;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import com.google.zxing.WriterException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
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
            String TutorialSurveyURl = "localhost:8080/survey?type=TUTORIAL&id="+ course.get().getId();   //TODO: replace localhost:8080 with domain name
            String SeminarSurveyURl = "localhost:8080/survey?type=SEMINAR&id="+ course.get().getId();


            //send byte array (the QRCode image) to model 
            model.addAttribute("LectureQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(LectureSurveyURl)));  
            model.addAttribute("TutorialQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(TutorialSurveyURl)));
            model.addAttribute("SeminarQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(SeminarSurveyURl)));  
            
            return "courseDetails";
        } else {
			return "redirect:../courses";
		}
    }

        




    //Create Course
    @GetMapping("course/new")
	public String createCourse(Model model, CourseForm form) {

        model.addAttribute("form", form);

        //List of Semesters for Course Creator to pick from
        model.addAttribute("semesterDates", courseManagement.findSemesters());
		return "courseNew";
	}



    //Validation of Created course
	@PostMapping("course/new")
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

            // if type is none of the correct values, then redirect to homepage
            if(!(type.equals("LECTURE")) && !(type.equals("TUTORIAL")) && !(type.equals("SEMINAR"))){
                //TODO: Where to go from here? Maybe send exception to trigger popup?
                return "redirect:/";

            }

            else{
                //Method from courseManager which sets the survey for the relevant surveyType
                System.out.println("THIS worked");
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

    //Mapping for Survey HTML
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


    //Mapping to recieve SURVEY (Formatted as JSON) from server
    @GetMapping("/survey/get")
    @ResponseBody
    public String sendSurvey( @RequestParam String type, @RequestParam(required = false) String id){

        //redirect 
        if (id == null) {
			return null;
        }

        else{
            //Retrieve survey
            String JsonString = courseManagement.getSurveyforType(id, type);

            //Remove [] so JS method can parse the JSON correctly
            JsonString = JsonString.substring(1,JsonString.length()-1);

            //return the JSON
            return JsonString;
        }
    }

    //---------------------------------------------------------------------------
   




    //to test
    //http://localhost:8080/qrcode?type=LECTURE&id=c000000000000001

   
    /**
     * Returns a HttpEntity (QRCode) of type PNG
     * @param response
     * @param type
     * @param id
     * @return
     * @throws IOException
     * @throws WriterException
     */
    @GetMapping("/qrcode")
    public HttpEntity<byte[]> qrcode(HttpServletResponse response, @RequestParam String type, @RequestParam(required = false) String id) throws IOException, WriterException  {

        //QRCode URL (Redirects to a courses survey when scanned). Generated using pathvariables
        String url = "localhost:8080/survey?type=" + type + "&id=" + id;  
        
        //find course
        Optional<Course> crs = courseRepository.findById(id);

        //generate filename
        String filename = crs.get().getName() + type + "QRCode";

        //Generate QRCode
        byte[] qrcode = courseManagement.generateQRCodeImage(url);

        //Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.IMAGE_PNG);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(qrcode.length);

        return new HttpEntity<byte[]>(qrcode, header);
    }
    
















































    //test method
    @GetMapping("/createC")
    public String createC(){
        courseManagement.TestCreateCourse();
        return "home";
    }

    //test method
    @GetMapping("/jsTest")
    public String JsTest(){
        return "survey";
    }


}


