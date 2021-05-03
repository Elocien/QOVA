package qova.logic;

import java.io.IOException;
import java.util.*;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import com.google.zxing.WriterException;

import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.admin.AdminManagement;
import qova.admin.DefaultSurvey;
import qova.enums.CourseType;
import qova.forms.CourseForm;
import qova.forms.DuplicateCourseForm;
import qova.forms.InstanceTitleForm;
import qova.forms.SurveyForm;
import qova.objects.Course;
import qova.objects.CourseInstance;

/**
 * <p>The class responsible for all mappings relating to the subdomain ../course</p>
 *
 * <p>All mappings under the ../course subdomain relate to functionality only available to users with the STAFF role. The main features captured in this controller
 * pertain to basic crud functions for the course object as well as viewing details. Further mappings in the controller cover questioneditor operations,
 * including editing and viewing a preview of the questionnaire</p>
 *
 * <p>The controller contains as little business logic as possible, as this is contained in the {@linkplain CourseManagement} class. </p>
 *
 */
@Controller
public class CourseController {

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final AdminManagement adminManagement;

    @Autowired
    CourseController(CourseManagement courseManagement, ResponseManagement responseManagement,
            AdminManagement adminManagement) {
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.adminManagement = Objects.requireNonNull(adminManagement);
    }

    // Error codes
    int courseNotFound = 1;

    /**
     * Mapping for displaying all {@linkplain Course}s a user has created. The page returned also allows users to start course creation
     * @param model {@linkplain Model}
     * @param userDetails Used for retrieving the details of the {@linkplain qova.users.User}
     * @return the "courses" html page with the {@linkplain Course}s belonging to the {@linkplain qova.users.User}
     */
    @GetMapping("/course/list")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String courses(Model model, @AuthenticationPrincipal UserDetails userDetails) {

        String userId = userDetails.getUsername();

        model.addAttribute("courseList", courseManagement.findByOwnerid(userId));
        return "courses";
    }

    /**
     * The mapping allowing the viewing of individual {@linkplain Course}s. The page has inbuilt functionality to allow for the editing of a courses information.
     * @param model {@linkplain Model}
     * @param duplicateForm The form used when duplicating a {@linkplain Course}, via the {@linkplain #duplicateCourseWithNewSemester(DuplicateCourseForm, UUID)} method
     * @param form The form used when editing the information of a {@linkplain Course}, via the {@linkplain #editCourseValidation(Model, CourseForm, BindingResult, DuplicateCourseForm, UUID)} method
     * @param id The {@linkplain UUID} of the {@linkplain Course} being viewed
     * @return The html page "courseDetails" 
     * @throws Exception Thrown by the qrCode generation method {@linkplain CourseManagement#generateQRCodeImage(String)}
     */
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    @GetMapping("course/details")
    public String courseDetails(Model model, DuplicateCourseForm duplicateForm, CourseForm form,
            @RequestParam(required = false) UUID id) throws IOException, WriterException {

        // for editing purposes:
        model.addAttribute("semesterDates", courseManagement.findSemesters());
        model.addAttribute("form", form);
        model.addAttribute("duplicateForm", duplicateForm);

        // fetch course and go to details if present
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            Course course = crs.get();

            model.addAttribute("course", course);

            //Used for status flags
            model.addAttribute("surveysMissing", courseManagement.getNumberOfSurveysMissing(course));
            model.addAttribute("titlesMissing", courseManagement.getInstanceTitlesMissingFlag(course));

            //The DomainName
            String domainName = "localhost:8080";

            // QRCode URL (Redirects to a courses survey when scanned)

            String LectureSurveyUrl = domainName + "/survey/select?type=LECTURE&id=" + course.getId()+"&mode=participant";
            String TutorialSurveyUrl = domainName +  "/survey/select?type=TUTORIAL&id=" + course.getId()+"&mode=participant";
            String SeminarSurveyUrl = domainName + "/survey/select?type=SEMINAR&id=" + course.getId()+"&mode=participant";
            String PracticalSurveyUrl = domainName + "/survey/select?type=PRACTICAL&id=" + course.getId()+"&mode=participant";


            model.addAttribute("lectureLink", LectureSurveyUrl);
            model.addAttribute("tutorialLink", TutorialSurveyUrl);
            model.addAttribute("seminarLink", SeminarSurveyUrl);
            model.addAttribute("practicalLink", PracticalSurveyUrl);

            // send byte array (the QRCode image) to model
            model.addAttribute("lectureQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(LectureSurveyUrl)));
            model.addAttribute("tutorialQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(TutorialSurveyUrl)));
            model.addAttribute("seminarQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(SeminarSurveyUrl)));
            model.addAttribute("practicalQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(PracticalSurveyUrl)));

            return "courseDetails";
        } else {
            return "error";
        }
    }

    // Edit Course Validation (when course is updated, check wether the fields are
    // all appropriately set e.g. NotNull)

    /**
     * Edit Course Validation. Validates all fields in the {@linkplain CourseForm} using {@linkplain BindingResult} to check for errors in the supplied input.
     * If all fields are valid, the details of the {@linkplain Course} are updated.
     * @param model {@linkplain Model}
     * @param form The form used when editing the information of a {@linkplain Course}. Contains all relevant fields of the {@linkplain Course} object which
     *             are available for editing
     * @param result The result of the validation, containing any possible errors
     * @param duplcateCourseForm The form containing all relevant information required for duplicating a course
     * @param id The {@linkplain UUID} of the {@linkplain Course} being edited
     * @return The "courseDetails" html template in case of successful changes
     * @throws Exception From {{@link #courseDetails(Model, DuplicateCourseForm, CourseForm, UUID)}}
     */
    @PostMapping("course/edit")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String editCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
            BindingResult result, DuplicateCourseForm duplcateCourseForm, @RequestParam UUID id) throws IOException, WriterException {

        if (result.hasErrors()) {
            return courseDetails(model, duplcateCourseForm, form, id);
        }

        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            if (Boolean.TRUE.equals(crs.get().getFinalisedFlag())) {
                return courseDetails(model, duplcateCourseForm, form, id);
            }
            courseManagement.updateCourseDetails(id, form);
            return "redirect:/course/details?id=" + id;
        }

        return "redirect:/";

    }

    /**
     * Used in the courseDetails.html template to let the user set all surveys as default surveys, when attempting to finalise their course
     *
     * @param id {@linkplain UUID} of the {@linkplain Course}
     * @return The courseDetails template, with the surveyEditedFlag of the {@linkplain CourseInstance} set to true
     */
    @GetMapping("course/setDefaultSurvey")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String setDefaultSurvey(@RequestParam UUID id) {
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            var course = crs.get();
            for(CourseType courseType : CourseType.values()){
                var courseInstance = course.getInstance(courseType);
                if(courseInstance.isActive()){
                    courseManagement.setSurveyEditedFlagForCourseInstance(courseInstance);
                }
            }
            return "redirect:/course/details?id=" + id;
        }
        return "redirect:/course/list";

    }

    /**
     * The first step of course creation.
     * Mapping for creating new courses
     * @param model {@linkplain Model}
     * @param form {@linkplain CourseForm} conatining all relevant fields for course creation
     * @return "courseNew" html template
     */
    @GetMapping("course/new")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String createCourse(Model model, CourseForm form) {

        model.addAttribute("form", form);

        // List of Semesters for Course Creator to pick from
        model.addAttribute("semesterDates", courseManagement.findSemesters());
        return "courseNew";
    }

    /**
     * The postMapping used to validate the details entered by the user and serialise the course object, as well as persist it in the database
     *
     * @param model model {@linkplain Model}
     * @param form form {@linkplain CourseForm} conatining all relevant fields for course creation
     * @param result The result of the validation on the fields of the {@linkplain CourseForm}
     * @param userDetails  Used for retrieving the details of the {@linkplain qova.users.User}
     * @return The "instanceTitles" html template for the newly created course, allowing the user to set the title for the number of given instances
     */
    @PostMapping("course/new")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String createCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
                                         BindingResult result,  @AuthenticationPrincipal UserDetails userDetails) {

        if (result.hasErrors()) {
            return createCourse(model, form);
        }

        // Get DefaultSurvey to reference in CourseInstance
        EnumMap<CourseType, DefaultSurvey> defaultSurveyMap = new EnumMap<>(CourseType.class);
        for(CourseType courseType : CourseType.values()){
            defaultSurveyMap.put(courseType, adminManagement.getDefaultSurveyObject(courseType));
        }

        String userId = userDetails.getUsername();

        if(userId.isEmpty()){
            return "redirect:/";
        }

        // Management Method returns String of new Course
        UUID id = courseManagement.createCourseAndCourseInstanceAndReturnCourseId(userId, form, defaultSurveyMap);

        // Redirect to SurveyEditor to start creating survey
        return "redirect:../course/instanceTitles?id=" + id;
    }

    /**
     * The second step of course creation
     * The user sets the titles for each of the instances of the course
     * Model attributes are added so a javascript function can dynamically display the right number of fields for the chosen {@linkplain CourseType}s
     * @param model model {@linkplain Model}
     * @param form The {@linkplain InstanceTitleForm} containing fields for setting the titles
     * @param id {@linkplain UUID} of the {@linkplain Course}
     * @return The "instanceTitles" html template with relevant model fields
     */
    @GetMapping("course/instanceTitles")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String createCourseSetInstanceTitles(Model model, @ModelAttribute("form") InstanceTitleForm form,
            @RequestParam UUID id) {

        Optional<Course> crs = courseManagement.findById(id);

        if (crs.isPresent()) {

            Course course = crs.get();

            model.addAttribute("id", crs.get().getId());

            if (Boolean.TRUE.equals(crs.get().getLectureExists())) {
                model.addAttribute("lectureExists", true);
                model.addAttribute("lectureInstances", course.getLecture().getInstanceAmount());
                model.addAttribute("lectureInstanceTitles", course.getLecture().getInstanceTitles());
            } else {
                model.addAttribute("lectureExists", false);
            }

            if (Boolean.TRUE.equals(crs.get().getTutorialExists())) {
                model.addAttribute("tutorialExists", true);
                model.addAttribute("tutorialInstances", course.getTutorial().getInstanceAmount());
                model.addAttribute("tutorialInstanceTitles", course.getTutorial().getInstanceTitles());
            } else {
                model.addAttribute("tutorialExists", false);
            }

            if (Boolean.TRUE.equals(crs.get().getSeminarExists())) {
                model.addAttribute("seminarExists", true);
                model.addAttribute("seminarInstances", course.getSeminar().getInstanceAmount());
                model.addAttribute("seminarInstanceTitles", course.getSeminar().getInstanceTitles());
            } else {
                model.addAttribute("seminarExists", false);
            }

            if (Boolean.TRUE.equals(crs.get().getPracticalExists())) {
                model.addAttribute("practicalExists", true);
                model.addAttribute("practicalInstances", course.getPractical().getInstanceAmount());
                model.addAttribute("practicalInstanceTitles", course.getPractical().getInstanceTitles());
            } else {
                model.addAttribute("practicalExists", false);
            }
        }

        return "instanceTitles";
    }

    /**
     * Validation of instanceTitles set by the user. Instance titles are sent via JSON, as forms can't have a dynamic number of fields, as well as thymeleaf having
     * problems with arrays. The JSON parsing is done inside of the {@linkplain InstanceTitleForm}.
     * @param model model {@linkplain Model}
     * @param form The {@linkplain InstanceTitleForm} containing fields for setting the titles
     * @param id {@linkplain UUID} of the {@linkplain Course}
     * @param result The result of the validation on the fields of the {@linkplain InstanceTitleForm}
     * @return The "courseDetails" html template of the newly created course
     */
    @PostMapping("course/instanceTitles")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String createCourseSetInstanceTitlesValidation(Model model, InstanceTitleForm form, @RequestParam UUID id,
            BindingResult result) {

        // Management Method returns String of new Course
        courseManagement.createCourseSetInstanceTitles(form, id);

        // Redirect to SurveyEditor to start creating survey
        return "redirect:../course/details" + "?id=" + id;
    }

    /**
     * Course deletion mapping
     * @param id {@linkplain UUID} of the {@linkplain Course}
     * @param userDetails Used for retrieving the details of the {@linkplain qova.users.User}                           
     * @return A redirect to {@linkplain #courses(Model, UserDetails)}
     */
    @GetMapping("course/delete")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String deleteCourse(@RequestParam UUID id, @AuthenticationPrincipal UserDetails userDetails) {
        if(courseManagement.findIfUserOwnsCourse(id, userDetails.getUsername())){
            courseManagement.deleteCourse(id);
        }
        return "redirect:../course/list";
    }

    /**
     * Mapping for duplicating a course. Main use case is to allow users to duplicate a course offered in the past, so as to not have to set all fields again
     * manually, only specify the new semester and thereby creating a new Course.
     * @param form The {@linkplain DuplicateCourseForm} containing all relevant fields for duplication
     * @param id {@linkplain UUID} of the {@linkplain Course}
     * @return The "courseDetails" html template of the new course
     */
    @PostMapping("course/duplicate")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String duplicateCourseWithNewSemester(@ModelAttribute("duplicateForm") DuplicateCourseForm form,
            @RequestParam UUID id) {

        Course newCourse = courseManagement.duplicateCourse(id, form.getSemesterString());

        return "redirect:../course/details?id=" + newCourse.getId();
    }

    /**
     * Mappping for finalising a course. Once a Course is finalised, all relevant objects are serialised in the database, allowing results to be captured.
     * Finalisation is more described in more detail in the documentation
     * @param id {@linkplain UUID} of the {@linkplain Course}
     * @return The "courseDetails" html template of the finalised course (reloads the same page essentially)
     */
    @GetMapping("course/finalise")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String finaliseCourse(@RequestParam UUID id) {

        courseManagement.setCourseFinalised(id);

        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {
            for (CourseType courseType : CourseType.values()) {

                String completeSurvey = adminManagement.concatenateDefaultSurveyToSurveyString(
                        courseManagement.getSurveyforType(id, courseType), courseType);

                try {
                    new JSONArray(completeSurvey);
                } catch (Exception e) {
                    return "redirect:../course/details" + "?id=" + id;
                }

                // Create JSON Array
                JSONArray survey = new JSONArray(completeSurvey);

                // Create the relevant objects
                responseManagement.createSurveyResponse(survey, course.get(), courseType);
            }
        }

        return "redirect:../course/details?id=" + id;
    }

    // Call Questioneditor and Submit created Survey
    // ---------------------------------------------------------------------------

    /**
     * Mapping for surveyeditor HTML (called from CourseDetails Page!). The questioneditor is used for adding custom questions to the
     * default questionnaire. Most of the functionality occurs in the template via javascript.
     *
     * @param model {@link org.springframework.ui.Model}
     * @param type  {@linkplain qova.enums.CourseType} in String form
     * @param id    Id of the Course
     * @return questioneditor.html template
     */
    @GetMapping("course/surveyeditor")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String questioneditor(Model model, @RequestParam String type, @RequestParam(required = false) UUID id) {


        // give course name to model, to show as title
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {
            model.addAttribute("coursename", course.get().getName());

            // Give model the following attributes, which are used to submit the survey, via
            // the post method
            model.addAttribute("typeID", type);
            model.addAttribute("id", id);

            // Gives the survey JSON to the model, so the current survey can be assembled
            // and added to
            model.addAttribute("survey", courseManagement.getSurveyforType(id, responseManagement.parseCourseType(type)));

            // Default survey JSON, which is sent to the server
            model.addAttribute("defaultSurvey", adminManagement.getDefaultSurvey(responseManagement.parseCourseType(type)));


            return "questioneditor";
        }
        else{
            return "redirect:/";
        }

    }

    /**
     * Mapping for submitting a created survey. The questioneditor sends JSON
     * containing the survey to the server, and this is checked for length (Can't
     * exceed 100 questions) and special characters. If the
     * {@linkplain qova.objects.CourseInstance} hasn't had a survey set before, then
     * the flag is set for that given {@linkplain qova.objects.CourseInstance}
     *
     * @param model The {@linkplain org.springframework.ui.Model}
     * @param form  {@linkplain SurveyForm} which contains the JSON passed by the
     *              surveyeditor
     * @param type  {@linkplain qova.enums.CourseType} in String format
     * @param id    Id of the {@linkplain Course}
     * @return Either the errorPage, in case of error; otherwise return the
     *         courseDetails template
     */
    @PostMapping("course/surveyeditor")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String questioneditorSubmit(Model model, @Valid @ModelAttribute("form") SurveyForm form,
            @RequestParam String type, @RequestParam(required = false) UUID id) {

        // fetch course
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {

            if ( Boolean.TRUE.equals(course.get().getFinalisedFlag())){
                return "redirect:/course/details" + "?id=" + id;
            }

            CourseType courseType = responseManagement.parseCourseType(type);


            // if type is none of the correct values, then redirect to error page
            if (courseType == null) {
                return "error";
            }

            CourseInstance instance = course.get().getInstance(courseType);

            if (Boolean.FALSE.equals(instance.getSurveyEditedFlag())) {
                courseManagement.setSurveyEditedFlagForCourseInstance(instance);
            }

            // check if JSON is valid
            try {
                new JSONArray(form.getQuestionnaireJson());
            } catch (Exception e) {
                return questioneditor(model, type, id);
            }

            // Create a JSON Array out of the response from the questioneditor
            JSONArray survey = new JSONArray(form.getQuestionnaireJson());

            // parse JSON to check for correctness (length, special characters)
            Boolean validSurvey = responseManagement.verifyJsonArray(survey);
            if (Boolean.FALSE.equals(validSurvey)) {
                return questioneditor(model, type, id);
            }

            // Sets the survey string for a given course (takes the default survey and
            // conncatenates it with the create survey)
            courseManagement.setSurveyforType(course.get(), type, form.getQuestionnaireJson());

            // Redirect back to CourseDetails page
            return "redirect:../course/details" + "?id=" + id;
        } else {
            return "error";
        }
    }

    // ---------------------------------------------------------------------------

    /**
     * The GetMapping to preview a survey created in the SurveyEditor by the user
     *
     * @param model The {@linkplain Model}
     * @param type The {@linkplain CourseType}
     * @param id The id of the {@linkplain Course}
     * @return The surveyPreview template
     */
    @GetMapping("course/previewsurvey")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String questioneditorpreviewget(Model model, @RequestParam String type,
                                           @RequestParam(required = false) UUID id) {

        // fetch course
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {

            // if type is none of the correct values, then redirect to homepage
            if (responseManagement.parseCourseType(type) == null) {
                return "error";
            }

            model.addAttribute("typeID", type);
            model.addAttribute("id", id);
            model.addAttribute("survey",
                    courseManagement.getSurveyforType(id, responseManagement.parseCourseType(type)));
            model.addAttribute("defaultSurvey",
                    adminManagement.getDefaultSurvey(responseManagement.parseCourseType(type)));
            model.addAttribute("coursename", course.get().getName());
            model.addAttribute("finalised", course.get().getFinalisedFlag());

            return "surveypreview";
        } else {
            return "error";
        }
    }

    /**
     * The PostMapping for the surveyPreview. This functions the same as
     * {@linkplain CourseController#questioneditorSubmit(Model, SurveyForm, String, UUID)}. It sets the survey string
     * as well as the surveyEdited flag. However, instead of returning to the details page, it redirects to a preview of the survey
     *
     * @param model The {@linkplain Model}
     * @param type The {@linkplain CourseType}
     * @param id The id of the {@linkplain Course}
     * @param form {@linkplain SurveyForm} containing the surveyJson from the submission
     * @return The surveyPreview template
     */
    @PostMapping("course/previewsurvey")
    @PreAuthorize("hasAnyRole('STAFF','ADMIN')")
    public String questioneditorpreview(Model model, SurveyForm form, @RequestParam String type,
            @RequestParam(required = false) UUID id) {

        // fetch course
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {

            //If the Course is finalised, then it is unable to be edited
            if ( Boolean.TRUE.equals(course.get().getFinalisedFlag())){
                return "redirect:/course/details" + "?id=" + id;
            }

            CourseType courseType = responseManagement.parseCourseType(type);

            // if type is none of the correct values, then redirect to homepage
            if (courseType == null) {

                return "error";
            }

            CourseInstance instance = course.get().getInstance(courseType);

            //
            if (Boolean.FALSE.equals(instance.getSurveyEditedFlag())) {
                courseManagement.setSurveyEditedFlagForCourseInstance(instance);
            }

            // check if JSON is valid
            try {
                new JSONArray(form.getQuestionnaireJson());
            } catch (Exception e) {
                return questioneditor(model, type, id);
            }

            // Create a JSON Array out of the response from the questioneditor
            JSONArray survey = new JSONArray(form.getQuestionnaireJson());

            // parse JSON to check for correctness (length, special characters)
            Boolean validSurvey = responseManagement.verifyJsonArray(survey);
            if (Boolean.FALSE.equals(validSurvey)) {
                // TODO: redirect to error page with code 02
                return questioneditor(model, type, id);
            }

            // Sets the survey string for a given course (takes the default survey and
            // conncatenates it with the create survey)
            courseManagement.setSurveyforType(course.get(), type, form.getQuestionnaireJson());


            // Part der anders ist als questioneditorSubmit
            model.addAttribute("typeID", responseManagement.parseCourseType(type));
            model.addAttribute("id", id);

            return "redirect:../course/previewsurvey?id=" + id + "&type=" + type;
        } else {
            // TODO: need more feedback here for the user. Change this!
            return "error?code=" + courseNotFound;
        }
    }


    /**
     * This method takes id and CourseType as parameters, and returns a qrcode as a {@linkplain HttpEntity}
     *
     * @param response {@link javax.servlet.http.HttpServletResponse}
     * @param type     {@linkplain qova.enums.CourseType}
     * @param id       Id of the {@linkplain Course}
     * @return The QRCode as a byte[], bundled into a {@link HttpEntity}
     * @throws IOException     Thrown by QRCode generator
     * @throws WriterException Thrown by QRCode generator
     */
    @GetMapping("qrcode")
    public HttpEntity<byte[]> qrcode(HttpServletResponse response, @RequestParam String type, @RequestParam UUID id)
            throws IOException, WriterException {

        return courseManagement.qrCodeAsHttpEntity(type, id);
    }
}
