package qova.response;

import qova.AbstractIntegrationTest;
import qova.course.Course;
import qova.course.CourseFaculty;
import qova.course.CourseInstance;
import qova.course.CourseType;
import qova.responseTypes.AbstractResponse;
import qova.responseTypes.BinaryResponse;
import qova.responseTypes.MultipleChoiceResponse;
import qova.responseTypes.ResponseType;
import qova.responseTypes.SingleChoiceResponse;
import qova.responseTypes.SurveyResponse;
import qova.responseTypes.TextResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

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

        var type = CourseType.LECTURE;
        var instanceNumber = 12;
        var groupNumber = 4;

        List<AbstractResponse> responses = new ArrayList<>();

        BinaryResponse bnr = new BinaryResponse("Would you consider recommending the lecture to other students?");
        for(int i = 0; i < 50 ; i++){bnr.incrementYes();}
        for(int i = 0; i < 25 ; i++){bnr.incrementNo();}


        
        
        ArrayList<String> mcOptions = new ArrayList<String>();
        mcOptions.add("1");
        mcOptions.add("2");
        mcOptions.add("3");
        mcOptions.add("4");
        mcOptions.add("5");
        MultipleChoiceResponse mcr = new MultipleChoiceResponse("From 1 to 5, what would you rate the lecture?", mcOptions);

        ArrayList<Integer> mcAnswers1 = new ArrayList<Integer>();
        mcAnswers1.add(0);
        mcAnswers1.add(1);
        mcAnswers1.add(3);

        ArrayList<Integer> mcAnswers2 = new ArrayList<Integer>();
        mcAnswers2.add(1);
        mcAnswers2.add(4);


        ArrayList<Integer> mcAnswers3 = new ArrayList<Integer>();
        mcAnswers3.add(2);
        mcAnswers3.add(4);

        for(int i = 0; i < 25 ; i++){mcr.incrementTotals(mcAnswers1);}
        for(int i = 0; i < 15 ; i++){mcr.incrementTotals(mcAnswers2);}
        for(int i = 0; i < 10 ; i++){mcr.incrementTotals(mcAnswers3);}

        


        TextResponse txr = new TextResponse("What is your opinion of the lecture, is it helpful?");
        for(int i = 0; i < 20 ; i++){txr.addTextSubmission("this is a bit of a test");}
        for(int i = 0; i < 10 ; i++){txr.addTextSubmission("this is a larger test to test the test");}
        for(int i = 0; i < 17 ; i++){txr.addTextSubmission("short test");}
        for(int i = 0; i < 3 ; i++){txr.addTextSubmission("this is a very very very very very very very very very very very very very very very very very very very very large test");}


        responses.add(bnr);
        responses.add(mcr);
        responses.add(txr);
        SurveyResponse rsp = new SurveyResponse(crs, type, instanceNumber, groupNumber, responses);
        
        

        assertEquals(crs, rsp.getCourse());
        assertEquals(type, rsp.getCourseType());
        assertEquals(groupNumber, rsp.getGroupNumber());
        assertEquals(instanceNumber, rsp.getInstanceNumber());
        // assertEquals(expected, rsp.getNumberOfSubmissions());
        assertEquals(responses, rsp.getUserResponses());
        
    }

    @Test
    public void BinaryResponseConstructorTest(){
        
        var question = "Was the lecture informative";
        
        BinaryResponse br = new BinaryResponse(question);
        br.incrementYes();
        br.incrementNo();
        br.incrementNo();
        
        assertEquals(question, br.getQuestion());
        assertEquals(br.getYesTotal(), 1);
        assertEquals(br.getNoTotal(), 2);
        assertEquals(ResponseType.BINARY_ANSWER, br.getType());
    }

    @Test
    public void TextResponseConstructorTest(){
        
        var question = "Was the lecture informative";
        
        TextResponse tr = new TextResponse(question);
        
        assertEquals(question, tr.getQuestion());
        assertEquals(ResponseType.TEXT_RESPONSE, tr.getType());
    }

    @Test
    public void SingleChoiceConstructorTest() throws Exception {
        
        String question = "Rate the lecutre from 1 to 5";

        ArrayList<String> scOptions = new ArrayList<String>();
        scOptions.add("1");
        scOptions.add("2");
        scOptions.add("3");
        scOptions.add("4");
        scOptions.add("5");
        SingleChoiceResponse scr = new SingleChoiceResponse(question, scOptions);


        for(int i = 0; i < 3 ; i++){scr.incrementTotal(0);}
        for(int i = 0; i < 8; i++){scr.incrementTotal(1);}
        for(int i = 0; i < 16 ; i++){scr.incrementTotal(2);}
        for(int i = 0; i < 15 ; i++){scr.incrementTotal(3);}
        for(int i = 0; i < 8 ; i++){scr.incrementTotal(4);}


        ArrayList<Integer> totals = new ArrayList<Integer>();
        totals.add(3);
        totals.add(8);
        totals.add(16);
        totals.add(15);
        totals.add(8);

        assertEquals(question, scr.getQuestion());
        assertEquals(scOptions, scr.getSingleChoiceOptions());
        assertEquals(totals, scr.getSingleChoiceAnswers());
        assertEquals(ResponseType.SINGLE_CHOICE, scr.getType());
    }

    @Test
    public void MultipleChoiceConstructorTest() throws Exception {
        
        String question = "What was good about the lecture (multiple options can be selected)";

        ArrayList<String> mcOptions = new ArrayList<String>();
        mcOptions.add("It was informative");
        mcOptions.add("It was interesting");
        mcOptions.add("I learned something new");
        mcOptions.add("I enjoyed attending the lecture");
        mcOptions.add("I would recommend the lecture to others");
        MultipleChoiceResponse mcr = new MultipleChoiceResponse(question, mcOptions);

        ArrayList<Integer> mcAnswers1 = new ArrayList<Integer>();
        mcAnswers1.add(0);
        mcAnswers1.add(1);
        mcAnswers1.add(3);

        ArrayList<Integer> mcAnswers2 = new ArrayList<Integer>();
        mcAnswers2.add(1);
        mcAnswers2.add(4);


        ArrayList<Integer> mcAnswers3 = new ArrayList<Integer>();
        mcAnswers3.add(2);
        mcAnswers3.add(4);

        for(int i = 0; i < 25 ; i++){mcr.incrementTotals(mcAnswers1);}
        for(int i = 0; i < 15 ; i++){mcr.incrementTotals(mcAnswers2);}
        for(int i = 0; i < 10 ; i++){mcr.incrementTotals(mcAnswers3);}


        //arraylist containing the totals set above, used to check against actual arraylist
        ArrayList<Integer> totals = new ArrayList<Integer>(mcOptions.size());
        totals.add(25);
        totals.add(40);
        totals.add(10);
        totals.add(25);
        totals.add(25);

        assertEquals(question, mcr.getQuestion());
        assertEquals(mcOptions, mcr.getMutltipleChoiceOptions());
        assertEquals(totals, mcr.getMutltipleChoiceAnswers());
        assertEquals(5, mcr.getNumberOfOptions());
        assertEquals(ResponseType.MULTIPLE_CHOICE, mcr.getType());
    }

}


