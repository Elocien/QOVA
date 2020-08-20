package qova.logic;

import java.io.IOException;
import java.util.Base64;
import java.util.Objects;

import java.util.Optional;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import com.google.zxing.WriterException;

import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.admin.AdminManagement;
import qova.forms.CourseForm;
import qova.forms.InstanceTitleForm;
import qova.forms.SurveyForm;
import qova.objects.Course;

//TODO: Temporary Imports (can be removed later)
//@Lucian please don't delete me just yet T_T
import java.util.List;
import java.util.Arrays;
import java.time.LocalDate;
import java.time.LocalTime;
//END


@Controller // This means that this class is a Controller
public class CourseController {


    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final AdminManagement adminManagement;

    @Autowired
    CourseController(CourseManagement courseManagement, ResponseManagement responseManagement, AdminManagement adminManagement) {
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.adminManagement = Objects.requireNonNull(adminManagement);
    }

    //Error codes
    int courseNotFound = 1;
    int internalError = 2;



    //General Pages (relevant domain wide)
    //-------------------------------------------------------
    
    @GetMapping("/")
    public String welcome () {
        return "home";
    }

    @GetMapping("error")

    public String error (Model model, @PathVariable(required = false) Integer code) {

        
        //add error code to model
        model.addAttribute("errorCode", code);

        //return template
        return "error";
    }


    //-------------------------------------------------------


    //Shows a table containing all courses 
    @GetMapping("courses")
    public String courses (Model model) {

        model.addAttribute("courseList", courseManagement.findAll());
        return "courses";
    }

    /*@GetMapping("/courses")
    public String courses (Model model) {
        
        List<Course> courseList = Arrays.asList(courseManagement.TimTestCreateCourse(), courseManagement.TimTestCreateCourse());
        model.addAttribute("courseList", courseList);

        return "courses";
    }*/



    //Shows the details for a specific course
    @GetMapping("course/details")
    public String courseDetails(Model model, CourseForm form, @RequestParam(required = false) String id) throws Exception {
        
        //for editing purposes:
        model.addAttribute("semesterDates", courseManagement.findSemesters());
        model.addAttribute("form", form);

        //redirect 
        if (id == null) {
			return "error?code=" + courseNotFound;
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()){
            model.addAttribute("course", course.get());


            //QRCode URL (Redirects to a courses survey when scanned)
            String LectureSurveyURl = "localhost:8080/surveySelect?type=LECTURE&id="+ course.get().getId();
            String TutorialSurveyURl = "localhost:8080/survey?type=TUTORIAL&id="+ course.get().getId();   //TODO: replace localhost:8080 with domain name
            String SeminarSurveyURl = "localhost:8080/survey?type=SEMINAR&id="+ course.get().getId();
            String PracticalSurveyURL = "localhost:8080/survey?type=PRACTICAL&id="+ course.get().getId();


            //send byte array (the QRCode image) to model 
            model.addAttribute("LectureQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(LectureSurveyURl)));  
            model.addAttribute("TutorialQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(TutorialSurveyURl)));
            model.addAttribute("SeminarQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(SeminarSurveyURl)));
            model.addAttribute("PracticalQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(PracticalSurveyURL)));  
            
            return "courseDetails";
        } else {
			return "error?code=" + courseNotFound;
		}
    }

    /*@GetMapping("/course/details")
    public String courseDetails(Model model, CourseForm form, String id) {

        id = "5";
        model.addAttribute("course", courseManagement.TimTestCreateCourse());
        model.addAttribute("form", form);
        model.addAttribute("semesterDates", courseManagement.findSemesters());

        return "courseDetails";
    }*/



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
		return "redirect:../course/new2?id=" + id;
    }


    //Create Course
    @GetMapping("course/new2")
	public String createCourseSetInstanceTitles(Model model, InstanceTitleForm form, @RequestParam String id) {

        model.addAttribute("form", form);

        model.addAttribute("lectureInstanceTitles", courseManagement.findById(id).get().getLecture().getInstanceTitles());
        model.addAttribute("tutorialInstanceTitles", courseManagement.findById(id).get().getTutorial().getInstanceTitles());
        model.addAttribute("seminarInstanceTitles", courseManagement.findById(id).get().getSeminar().getInstanceTitles());
        model.addAttribute("practicalInstanceTitles", courseManagement.findById(id).get().getPractical().getInstanceTitles());

        model.addAttribute("lectureInstances", courseManagement.findById(id).get().getLecture().getInstanceAmount());
        model.addAttribute("tutorialInstances", courseManagement.findById(id).get().getTutorial().getInstanceAmount());
        model.addAttribute("seminarInstances", courseManagement.findById(id).get().getSeminar().getInstanceAmount());
        model.addAttribute("practicalInstances", courseManagement.findById(id).get().getPractical().getInstanceAmount());
        
        model.addAttribute("lectureExists", true);
        model.addAttribute("tutorialExists", true);
        model.addAttribute("seminarExists", true);
        model.addAttribute("practicalExists", true);

		return "courseNew2";
    }

    /*@GetMapping("course/new2")
	public String createCourse_SetInstanceTitles(Model model, InstanceTitleForm form) {

        model.addAttribute("lectureInstances", 2);
        model.addAttribute("lectureExists", true);

        model.addAttribute("tutorialInstances", 3);
        model.addAttribute("tutorialExists", true);

        model.addAttribute("seminarInstances", 2);
        model.addAttribute("seminarExists", false);

        model.addAttribute("practicalInstances", 5);
        model.addAttribute("practicalExists", true);

		return "courseNew2";
    }*/
    
    //Validation of Created course
	@PostMapping("course/new2")
	public String createCourseSetInstanceTitlesValidation(Model model, @Valid @ModelAttribute("form") InstanceTitleForm form, @RequestParam String id,
			BindingResult result) throws Exception {


		if (result.hasErrors()) {
			return createCourseSetInstanceTitles(model, form, id);
        }



        //Management Method returns String of new Course
        courseManagement.createCourseSetInstanceTitles(form, id);
        
        //Redirect to SurveyEditor to start creating survey
		return "redirect:../course/details" + "?id=" + id;
    }

    
    



    //Delete Course 
    @PostMapping("course/delete")
	public String deleteCourse(@RequestParam String id) {
		courseManagement.deleteCourse(id);
		return "redirect:../courses";
    }
    


    
    //Edit Course Validation (when course is updated, check wether the fields are all appropriately set e.g. NotNull)
    @PostMapping("course/edit")
	public String editCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
			BindingResult result, @RequestParam String id) throws Exception {

		if (result.hasErrors()) {
			return courseDetails(model, form, id);
		}

		courseManagement.updateCourseDetails(id, form);
		return "redirect:../course/details?id=" + id;
	}



    //Call Questioneditor and Submit created Survey
    //---------------------------------------------------------------------------


    
    /** Mapping for surveyeditor HTML (called from CourseDetails Page!)
     * 
     * @param model
     * @param type
     * @param id
     * @return
     * @throws Exception
     */
    @GetMapping("course/surveyeditor")
    public String questioneditor(Model model, @RequestParam String type, @RequestParam(required = false) String id)
            throws Exception {
        
        //Give model the following attributes, which are used to submit the survey, via the post method
        model.addAttribute("typeID", type);
        model.addAttribute("id", id);

        //Gives the survey JSON to the model, so the current survey can be assembled and added to
        model.addAttribute("survey", courseManagement.getSurveyforType(id, type));

        //Default survey JSON, which is sent to the server
        model.addAttribute("defaultSurvey", adminManagement.getDefaultSurvey());


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


     
    /**
     * Mapping for submitting a created survey. The questioneditor sends JSON
     * containing the survey to the server, and this is checked for length (Can't
     * exceed 100 questions) and special characters. After this, a new
     * SurveyResponse object is created along with its subclasses (BinaryResponse,
     * TextResponse, etc.)
     * 
     * @param form
     * @param type
     * @param id
     * @return
     * @throws Exception
     */
    @PostMapping("course/surveyeditor")
    public String questioneditorSubmit(SurveyForm form, @RequestParam String type,
            @RequestParam(required = false) String id) throws Exception {
        

        //Form empty -> Redirect to details again 
        if (form.getQuestionnairejson().length()==0) {
            return "redirect:../course/details" + "?id=" + id;          //TODO: Redirects back course at the moment, think about where this should go
        }
        

        //fetch course 
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()){

            // if type is none of the correct values, then redirect to homepage
            if(responseManagement.parseCourseType(type) == null){
                //TODO: redirect to error page with code 02
                return "redirect:/";
            }

            else{
                //check if JSON is valid
                try {new JSONArray(form.getQuestionnairejson());} 
                catch (Exception e) {
                    //TODO: redirect to error page with code 02
                    return "redirect:/";
                }

                //Create a JSON Array out of the response from the questioneditor
                JSONArray survey = new JSONArray(form.getQuestionnairejson());

                //parse JSON to check for correctness (length, special characters)
                Boolean validSurvey = responseManagement.verifyJsonArray(survey);
                if(Boolean.FALSE.equals(validSurvey)){
                    //TODO: redirect to error page with code 02
                    return "redirect:/";
                }

                //Manager method for creating SurveyResponse and corresponding nested objects
                responseManagement.createSurveyResponse(survey, course.get(), type);

                //Sets the survey string for a given course (takes the default survey and conncatenates it with the create survey)
                courseManagement.setSurveyforType(course.get(), type, form.getQuestionnairejson(), adminManagement.getDefaultSurvey());
            }


            //Redirect back to CourseDetails page
            return "redirect:../course/details" + "?id=" + id;
        }
        else{
            //TODO: need more feedback here for the user. Change this!
            return "error?code=" + courseNotFound;
        }
    }
    

    //---------------------------------------------------------------------------




    //to test
    //http://localhost:8080/qrcode?type=LECTURE&id=c000000000000001

    
    /**
     * This method takes id and CourseType as parameters, and returns a qrcode with the given string that is assembled below
     * 
     * @param response HttpResponse
     * @param type
     * @param id
     * @return
     * @throws IOException
     * @throws WriterException
     */
    @GetMapping("qrcode")
    public HttpEntity<byte[]> qrcode(HttpServletResponse response, @RequestParam String type, @RequestParam String id) throws IOException, WriterException  {

        //QRCode URL (Redirects to a courses survey when scanned). Generated using pathvariables
        String url = "localhost:8080/survey?type=" + type + "&id=" + id;  
        
        //find course
        Optional<Course> crs = courseManagement.findById(id);

        //generate filename
        String filename = crs.get().getName() + "_" + type + "_" + "QRCode";

        //Generate QRCode
        byte[] qrcode = courseManagement.generateQRCodeImage(url);

        //Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.IMAGE_PNG);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(qrcode.length);

        return new HttpEntity<byte[]>(qrcode, header);
    }
    





    @GetMapping("/easterEgg/tim")
    public String easterEgg(){return "subject.html";}




































    //test method
    @GetMapping("/createC")
    public String createC() throws Exception {
        courseManagement.TestCreateCourse();
        return "home";
    }

    //test method
    @GetMapping("/jsTest")
    public String JsTest(){
        return "survey";
    }


}


