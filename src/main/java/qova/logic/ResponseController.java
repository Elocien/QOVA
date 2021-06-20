package qova.logic;

import java.util.*;

import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.springframework.ui.Model;

import qova.admin.AdminManagement;
import qova.enums.CourseType;
import qova.forms.SurveyForm;
import qova.forms.SurveySelectForm;
import qova.objects.Course;
import qova.objects.SurveyResponse;


@Controller // This means that this class is a Controller
public class ResponseController {

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final AdminManagement adminManagement;

    @Autowired
    ResponseController(ResponseManagement responseManagement, CourseManagement courseManagement,
            AdminManagement adminManagement) {
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.adminManagement = Objects.requireNonNull(adminManagement);
    }


    /**
     * <p>Mapping to which one is redirected to by the QRCode</p>
     * <p>Calls the surveySelect.html page, where students select for which instance and group they are submitting their evaluation for</p>
     * @param model {@linkplain Model}
     * @param form {@linkplain SurveySelectForm} which has fields for group and instance
     * @param mode Used to indicate where the user is redirected upon submission. Can be either participant to go to the survey, or results for surveyResults. 
     *             This functionality can be viewed in {@linkplain ResponseController#selectSurveySubmission(Model, SurveySelectForm, String, String, UUID)}
     * @param id {@linkplain Course} id
     * @param type {@linkplain CourseType}
     * @return surveySelect.html template
     */
    @GetMapping("/survey/select")
    @PreAuthorize("hasAnyRole('STAFF','STUDENT','ADMIN')")
    public String selectSurvey(Model model, SurveySelectForm form, @RequestParam String mode, @RequestParam UUID id,
            @RequestParam(required = false, defaultValue = "") String type) {

        // course name, course type, instance names, groupAmount
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {

            Course course = crs.get();

            model.addAttribute("course", course);
            model.addAttribute("courseName", course.getName());
            model.addAttribute("id", course.getId());
            model.addAttribute("form", form);
            model.addAttribute("mode", mode);

            //Get the CourseType
            CourseType courseType = responseManagement.parseCourseType(type);
            //
            if (courseType == null) {
                model.addAttribute("typeExists", false);
            } else {
                model.addAttribute("typeExists", true);
                model.addAttribute("type", type);
                model.addAttribute("instanceTitles", course.getInstance(courseType).getInstanceTitles());
                model.addAttribute("groupAmount", course.getInstance(courseType).getGroupAmount());
            }

            return "surveySelect";
        }

        // if course does not exist, redirect to global error page
        return "error";
    }

    // ---------------------------------------------------------------------------


    /**
     * Validation of user entry on the surveySelect page. Redirects the user to the actual survey, or the results for the chosen survey
     * @param model {@linkplain Model}
     * @param form {@linkplain SurveySelectForm} which has fields for group and instance
     * @param mode Used to indicate where the user is redirected upon submission. Can be either participant to go to the survey, or results for surveyResults
     * @param type {@linkplain CourseType}
     * @param id {@linkplain Course} id
     * @return
     */
    @PostMapping("/survey/select")
    @PreAuthorize("hasAnyRole('STAFF','STUDENT','ADMIN')")
    public String selectSurveySubmission(Model model, @ModelAttribute("form") SurveySelectForm form,
            @RequestParam String mode, @RequestParam String type, @RequestParam UUID id) {

        Optional<Course> crs = courseManagement.findById(id);

        // if anything is null or not an allowed value, redirect back
        if (crs.isEmpty()) {
            return "error";
        }
        // if type is not one of the defined values
        if (responseManagement.parseCourseType(type) == null) {
            return "error";
        }

        else {

            if (mode.equals("participant")) {
                return "redirect:/survey/view?id=" + id + "&type=" + type + "&instance=" + form.getInstance() + "&group="
                        + form.getGroup();
            } else if (mode.equals("results")) {
                return "redirect:/survey/results?id=" + id + "&type=" + type + "&instance=" + form.getInstance() + "&group="
                        + form.getGroup();
            } else {
                return "error";
            }
        }
    }

    /**
     * Mapping for the survey itself. Calls the survey template and renders the survey from the json string in the frontend
     *
     * @param model {@linkplain Model}
     * @param id {@linkplain Course} id
     * @param type {@linkplain CourseType}
     * @param group The number of the group as an integer
     * @param instance The number of the instance as an integer
     * @return The survey.html template with the survey rendered in the frontend. Survey is retrieved with a get request after rendering template.
     */
    @GetMapping("/survey/view")
    @PreAuthorize("hasAnyRole('STAFF','STUDENT','ADMIN')")
    public String surveyView(Model model, @RequestParam(required = false) UUID id,
            @RequestParam(required = false) String type, @RequestParam(required = false) String group,
            @RequestParam(required = false) String instance) {


            // redirect
        if (id == null || type == null || group == null || instance == null) {
            return "error";
        }

        // fetch course and go to details if present
        Optional<Course> course = courseManagement.findById(id);

        // Validate that course exists, and that the survey is not empty
        if (course.isPresent()) {
            String survey = courseManagement.getSurveyforType(id, responseManagement.parseCourseType(type));
            if (survey.equals("Something went wrong")) {
                return "redirect:/";
            } else {

                // Concatenate the default survey to the course survey
                survey = adminManagement.concatenateDefaultSurveyToSurveyString(survey,
                        responseManagement.parseCourseType(type));

                model.addAttribute("group", group);
                model.addAttribute("instance", instance);
                model.addAttribute("typeID", type);
                model.addAttribute("id", id);
                model.addAttribute("survey", survey);
                model.addAttribute("coursename", course.get().getName());
                return "survey";
            }

        }

        // If condition not met, redirect to home
        else {
            return "error";
        }
    }


    /**
     * PostMapping to submit survey and save the users responses. Will reject the user response if they have already previously submitted to
     * the same survey for the given instance and group.
     *
     * @param model {@linkplain Model}
     * @param form {@linkplain SurveyForm} which contains the json string with the user response
     * @param id {@linkplain Course} id
     * @param type {@linkplain CourseType}
     * @param group The number of the group as an integer
     * @param instance The number of the instance as an integer
     * @param userDetails Used to retrieve the users credentials
     * @return surveyCheckout.html template
     */
    @PostMapping("/survey/view")
    @PreAuthorize("hasAnyRole('STAFF','STUDENT','ADMIN')")
    public String recieveResponseJSON(Model model, SurveyForm form, @RequestParam(required = false) UUID id,
            @RequestParam(required = false) String type, @RequestParam(required = false) String group,
            @RequestParam(required = false) String instance,  @AuthenticationPrincipal UserDetails userDetails) {

        if (id == null || type == null || group == null || instance == null) {
            return "error";
        }

        // Get the StudentId, through the authentication object
        String studentId = userDetails.getUsername();

        // Get the CourseType
        CourseType courseType = responseManagement.parseCourseType(type);

        // Initialise JSONArray
        JSONArray studentResponseJson;

        // get JSON Response
        try {
            studentResponseJson = new JSONArray(form.getQuestionnaireJson());
        } catch (Exception e) {
            return "error";
        }

        // fetch course
        Optional<Course> crs = courseManagement.findById(id);

        // Validate that course exists, and that the survey is not empty
        if (crs.isPresent()) {

            Course course = crs.get();


            // Concatenate the default survey to the course survey
            String fullSurvey = adminManagement.concatenateDefaultSurveyToSurveyString(course.getInstance(courseType).getSurvey(),
                    responseManagement.parseCourseType(type));

            // Verify that the sent JSON is not malicious. If malicious, returns false
            if(!responseManagement.verifyStudentResponseJson(studentResponseJson, fullSurvey)){

                //Return to the previous view
                return surveyView(model, id, type, group, instance);
            }

            Optional<SurveyResponse> survRsp = responseManagement
                    .findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, courseType,
                            Integer.valueOf(group), Integer.valueOf(instance));

            if (survRsp.isPresent()) {
                SurveyResponse surveyResponse = survRsp.get();

                // Redirect the user to SurveyReject, because they have already completed a survey
                if (surveyResponse.getListOfStudentsThatSubmitted().contains(studentId)){
                    model.addAttribute("id", id);
                    model.addAttribute("courseType", type);
                    return "redirect:/survey/reject?id=" + id + "&type=" + type;
                }

                // Increments Submission Counter
                surveyResponse.addStundentIdToSubmissionListAndIncrementCounter(studentId);

                // The manager method that increments & sets the correct values
                responseManagement.submitStudentResponse(surveyResponse, studentResponseJson);

            }

        }


        // if all goes well
        return "surveyCheckout";
    }

    // ---------------------------------------------------------------------------

    /**
     * Used to retrieve the results for a given questionnaire.
     * 
     * @param model         {@link org.springframework.ui.Model}
     * @param type          {@linkplain qova.enums.CourseType}
     * @param id            The Id of the {@linkplain qova.objects.Course}
     * @param group         The groupNumber of the {@linkplain qova.objects.Course}
     * @param instance      The instanceNumber of the {@linkplain qova.objects.Course}
     * @param userDetails   Used to retrieve the users Authorities
     * @return The surveyResults template, which shows the compiled results of the
     *         requested questionnaire
     */
    @GetMapping("/survey/results")
    @PreAuthorize("hasAnyRole('STAFF','STUDENT','ADMIN')")
    public String surveyResults(Model model, @RequestParam String type, @RequestParam UUID id,
            @RequestParam String group, @RequestParam String instance, @AuthenticationPrincipal UserDetails userDetails) {

        boolean userIsOwner = courseManagement.findIfUserOwnsCourse(id, userDetails.getUsername());

        model.addAttribute("instance", instance);
        model.addAttribute("group", group);
        model.addAttribute("id", id);


        // The Course object in an optional
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {

            CourseType courseType = responseManagement.parseCourseType(type);

            // The actual Course Object
            Course course = crs.get();

            // Eine Liste aller SurveyResponses
            List<SurveyResponse> listOfSurveyResponses = responseManagement.findSurveyResponses(course, courseType,
                    group, instance);

            JSONArray resultsJsonString = responseManagement.generateSurveyResultsJsonArray(listOfSurveyResponses, userIsOwner);

            Integer totalNumberOfSubmissions = responseManagement.getTotalResponses(listOfSurveyResponses);

            model.addAttribute("resultsJson", resultsJsonString.toString());
            model.addAttribute("courseName", course.getName());
            model.addAttribute("courseType", courseType);
            model.addAttribute("pdf_title", (course.getName()+"_"+courseType+"_G="+group.toString()+"_I="+instance+".pdf"));
            model.addAttribute("semester", course.getCourseDate());
            model.addAttribute("numberOfSubmissions", totalNumberOfSubmissions);

        }
        return "surveyResults";
    }

    /**
     * The Mapping where students can browse the full set of Courses and View the
     * results of the student evaluations
     * 
     * @param model {@link org.springframework.ui.Model}
     * @return studentBrowser template
     */
    @GetMapping("/survey/results/list")
    @PreAuthorize("hasAnyRole('STAFF','STUDENT','ADMIN')")
    public String studentBrowser(Model model) {
        model.addAttribute("courseList", courseManagement.findAll());

        return "studentBrowser";
    }

    /**
     * Page shown to the user after submission of a survey
     * @return surveyCheckout.html template
     */
    @GetMapping("/survey/checkout")
    public String surveyCheckout() {

        return "surveyCheckout";
    }

    /**
     * Page shown to the user after submitting to a survey for which they have submitted results for previously
     * @param model {@linkplain Model}
     * @param type {@linkplain CourseType}
     * @param id {@linkplain Course} id
     * @return surveyReject.html template
     */
    @GetMapping("/survey/reject")
    public String surveyReject(Model model, @RequestParam String type, @RequestParam UUID id) {
        model.addAttribute("mode", "participant");
        model.addAttribute("type", type);
        model.addAttribute("id", id);

        return "surveyReject";
    }


    @Deprecated
    @GetMapping("/survey/results/generatePDF")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public HttpEntity<byte[]> generatePdf(@RequestParam UUID id, @RequestParam String type,
            @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response)
            throws Exception {

        // generate filename
        String filename = "testPdf.pdf";

        Optional<Course> crs = courseManagement.findById(id);

        // verify that course is present
        if (crs.isEmpty()) {
            return null;
        }

        // Try to parse the courseType
        CourseType courseType = responseManagement.parseCourseType(type);
        if (courseType == null) {
            return null;
        }

        // Generate PDF
        byte[] pdf = responseManagement.generatePDFEnglish(crs.get(), courseType, Integer.parseInt(groupNumber),
                Integer.parseInt(instanceNumber));

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<>(pdf, header);
    }


    /**
     * CSV Generator used to retrieve results in CSV form
     *
     * @param id {@linkplain Course} id
     * @param type {@linkplain CourseType}
     * @param groupNumber Group as integer
     * @param instanceNumber Instance as integer
     * @return {@linkplain HttpEntity} containing the PDF
     * @throws Exception Writer exception from the pdf library
     */
    @GetMapping("/survey/results/generateCSV")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public HttpEntity<byte[]> generateCsv(@RequestParam UUID id, @RequestParam String type,
            @RequestParam String groupNumber, @RequestParam String instanceNumber)
            throws Exception {

        // generate filename
        String filename = "testCsv.csv";

        Optional<Course> crs = courseManagement.findById(id);

        // verify that course is present
        if (crs.isEmpty()) {
            return null;
        }

        // Try to parse the courseType
        CourseType courseType = responseManagement.parseCourseType(type);
        if (courseType == null) {
            return null;
        }

        // Generate CSV
        byte[] csv = responseManagement.generateCSVEnglish(
                responseManagement.findSurveyResponses(crs.get(), courseType, groupNumber, instanceNumber));

        if ( Arrays.equals(csv, new byte[0])) {
            return null;
        }

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(csv.length);

        return new HttpEntity<>(csv, header);
    }
}
