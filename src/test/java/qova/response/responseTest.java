package qova.response;

import qova.AbstractIntegrationTest;
import qova.course.Course;
import qova.course.CourseFaculty;
import qova.course.CourseInstance;
import qova.course.CourseType;
import qova.responseTypes.BinaryResponse;
import qova.responseTypes.MultipleChoiceResponse;
import qova.responseTypes.ResponseType;
import qova.responseTypes.SingleChoiceResponse;
import qova.responseTypes.TextResponse;
import qova.responseTypes.UserResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDate;
import java.util.ArrayList;

import org.junit.jupiter.api.Test;

public class responseTest extends AbstractIntegrationTest {
    @Test
    public void UserResponseConstructorTest() throws Exception {

        var name = "Rechnernetze";

        String[] lectureTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var lecture = new CourseInstance(CourseType.LECTURE, 1, 12, lectureTitles);

        String[] tutorialTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var tutorial = new CourseInstance(CourseType.TUTORIAL, 8, 12, tutorialTitles);

        CourseInstance seminar = null;

        CourseInstance practical = null;

        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        Course crs  = new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty, semesterString, courseDate);

        // ----------------------------------------------------------------------------------------------------------------------

        var courseType = CourseType.LECTURE;
        var classNo = 1;
        ArrayList<Object> responses = new ArrayList<>();


        BinaryResponse br = new BinaryResponse("Is this lecture good?", true);
        responses.add(br);

        TextResponse tr = new TextResponse("What are your thoughts on the lecture?", "It was a great lecture");
        responses.add(tr);


        ArrayList<String> singleChoiceOptions = new ArrayList<String>();
        singleChoiceOptions.add("Not informative");
        singleChoiceOptions.add("A little informative");
        singleChoiceOptions.add("Very informative");

        ArrayList<Boolean> singleChoiceAnswer = new ArrayList<Boolean>();
        singleChoiceAnswer.add(false);
        singleChoiceAnswer.add(true);
        singleChoiceAnswer.add(false);

        SingleChoiceResponse scr = new SingleChoiceResponse("How informative was the lecture?", singleChoiceOptions, singleChoiceAnswer);
        responses.add(scr);


        ArrayList<String> multipleChoiceOptions = new ArrayList<String>();
        multipleChoiceOptions.add("The lecture was informative");
        multipleChoiceOptions.add("The lecture was interesting");
        multipleChoiceOptions.add("I enjoyed attending the lecture");

        ArrayList<Boolean> multipleChoiceAnswers = new ArrayList<Boolean>();
        multipleChoiceAnswers.add(false);
        multipleChoiceAnswers.add(true);
        multipleChoiceAnswers.add(true);
        MultipleChoiceResponse mcr = new MultipleChoiceResponse("Which of these applies to the lecture you attended today?", multipleChoiceOptions, multipleChoiceAnswers);
        responses.add(mcr);


        UserResponse rsp = new UserResponse(crs, courseType, classNo, responses);


        assertEquals(courseType, rsp.getCourseType());
        assertEquals(classNo, rsp.getClassNo());
        assertEquals(crs, rsp.getCourse());
        assertEquals(responses, rsp.getUserResponse());
        
    }

    @Test
    public void BinaryResponseConstructorTest(){
        
        var question = "Was the lecture informative";
        var response = false;
        
        BinaryResponse br = new BinaryResponse(question, response);
        
        assertEquals(question, br.getQuestion());
        assertEquals(response, br.getResponse());
        assertEquals(ResponseType.BINARY_ANSWER, br.getType());
    }

    @Test
    public void TextResponseConstructorTest(){
        
        var question = "Was the lecture informative";
        var response = "yes, i enjoyed it as well";
        
        TextResponse tr = new TextResponse(question, response);
        
        assertEquals(question, tr.getQuestion());
        assertEquals(response, tr.getResponse());
        assertEquals(ResponseType.TEXT_RESPONSE, tr.getType());
    }

    @Test
    public void SingleChoiceConstructorTest() throws Exception {

        var question = "Was the lecture informative";

        ArrayList<String> singleChoiceOptions = new ArrayList<String>();
        singleChoiceOptions.add("Not informative");
        singleChoiceOptions.add("A little informative");
        singleChoiceOptions.add("Very informative");

        ArrayList<Boolean> singleChoiceAnswer = new ArrayList<Boolean>();
        singleChoiceAnswer.add(false);
        singleChoiceAnswer.add(true);
        singleChoiceAnswer.add(false);

        SingleChoiceResponse scr = new SingleChoiceResponse(question, singleChoiceOptions, singleChoiceAnswer);

        assertEquals(question, scr.getQuestion());
        assertEquals(singleChoiceOptions, scr.getMutltipleChoiceOptions());
        assertEquals(singleChoiceAnswer, scr.getMutltipleChoiceAnswer());
        assertEquals(ResponseType.SINGLE_CHOICE, scr.getType());
        
    }

    @Test
    public void MultipleChoiceConstructorTest() throws Exception {
        
        var question = "Was the lecture informative";

        ArrayList<String> multipleChoiceOptions = new ArrayList<String>();
        multipleChoiceOptions.add("The lecture was informative");
        multipleChoiceOptions.add("The lecture was interesting");
        multipleChoiceOptions.add("I enjoyed attending the lecture");

        ArrayList<Boolean> multipleChoiceAnswers = new ArrayList<Boolean>();
        multipleChoiceAnswers.add(false);
        multipleChoiceAnswers.add(true);
        multipleChoiceAnswers.add(true);

        MultipleChoiceResponse mcr = new MultipleChoiceResponse(question, multipleChoiceOptions, multipleChoiceAnswers);

        assertEquals(question, mcr.getQuestion());
        assertEquals(multipleChoiceOptions, mcr.getMutltipleChoiceOptions());
        assertEquals(multipleChoiceAnswers, mcr.getMutltipleChoiceAnswers());
        assertEquals(ResponseType.SINGLE_CHOICE, mcr.getType());
        
    }

}


