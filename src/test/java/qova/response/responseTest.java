package qova.response;

import qova.AbstractIntegrationTest;
import qova.enums.CourseFaculty;
import qova.enums.CourseType;
import qova.objects.BinaryResponse;
import qova.objects.Course;
import qova.objects.CourseInstance;
import qova.objects.MultipleChoiceResponse;
import qova.objects.SingleChoiceResponse;
import qova.objects.SurveyResponse;
import qova.objects.TextResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

public class responseTest extends AbstractIntegrationTest {
    
    @Test
    public void UserResponseConstructorTest() throws Exception {

        var name = "Rechnernetze";

        String[] lectureTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var lecture = new CourseInstance(CourseType.LECTURE, 1, 12, lectureTitles, true);

        String[] tutorialTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var tutorial = new CourseInstance(CourseType.TUTORIAL, 8, 12, tutorialTitles, true);

        CourseInstance seminar = null;

        CourseInstance practical = null;

        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        Course crs  = new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty, semesterString, courseDate);

        // ----------------------------------------------------------------------------------------------------------------------

        var type = CourseType.LECTURE;
        var instance = 1;
        var group = 1;

        SurveyResponse response = new SurveyResponse(crs, type, instance, group);

        BinaryResponse bnr = new BinaryResponse(response, "Would you consider recommending the lecture to other students?", 0);
        for(int i = 0; i < 35 ; i++){bnr.incrementYes();}
        for(int i = 0; i < 15 ; i++){bnr.incrementNo();}

        List<String> mcOptions = new ArrayList<>();
        mcOptions.add("1");
        mcOptions.add("2");
        mcOptions.add("3");
        mcOptions.add("4");
        mcOptions.add("5");
        MultipleChoiceResponse mcr = new MultipleChoiceResponse(response, "From 1 to 5, what would you rate the lecture?", 1, mcOptions);

        List<Integer> mcAnswers1 = new ArrayList<>();
        mcAnswers1.add(0);
        mcAnswers1.add(1);
        mcAnswers1.add(3);

        List<Integer> mcAnswers2 = new ArrayList<>();
        mcAnswers2.add(1);
        mcAnswers2.add(4);


        List<Integer> mcAnswers3 = new ArrayList<>();
        mcAnswers3.add(2);
        mcAnswers3.add(4);

        for(int i = 0; i < 25 ; i++){mcr.incrementTotals(mcAnswers1);}
        for(int i = 0; i < 15 ; i++){mcr.incrementTotals(mcAnswers2);}
        for(int i = 0; i < 10 ; i++){mcr.incrementTotals(mcAnswers3);}

        TextResponse txr = new TextResponse(response, "What is your opinion of the lecture, is it helpful?", 2);
        for(int i = 0; i < 20 ; i++){txr.addTextSubmission("this is a bit of a test");}
        for(int i = 0; i < 10 ; i++){txr.addTextSubmission("this is a larger test to test the test");}
        for(int i = 0; i < 17 ; i++){txr.addTextSubmission("short test");}
        for(int i = 0; i < 3 ; i++){txr.addTextSubmission("this is a very very very very very very very very very very very very very very very very very very very very large test");}


        List<String> listOfStudentIds = new ArrayList<>();
        for(int i = 0; i < 50; i++){
            String id = UUID.randomUUID().toString();
            response.addStundentIdToSubmissionListAndIncrementCounter(id);
            listOfStudentIds.add(id);
        }
        

        assertEquals(crs, response.getCourse());
        assertEquals(type, response.getCourseType());
        assertEquals(group, response.getGroupNumber());
        assertEquals(instance, response.getInstanceNumber());
        //The Maps order is not persisted in the same order the objects are added to it
        assertEquals(listOfStudentIds.containsAll(response.getListOfStudentsThatSubmitted()), response.getListOfStudentsThatSubmitted().containsAll(listOfStudentIds));
        assertEquals(50, response.getNumberOfSubmissions());
    }

    @Test
    public void BinaryResponseConstructorTest(){

        SurveyResponse surveyResponse = Mockito.mock(SurveyResponse.class);
        
        var question = "Was the lecture informative";
        
        BinaryResponse br = new BinaryResponse(surveyResponse, question, 0);
        br.incrementYes();
        br.incrementNo();
        br.incrementNo();
        
        assertEquals(question, br.getQuestion());
        assertEquals("1", br.getYesTotalString());
        assertEquals("2", br.getNoTotalString());
        assertEquals(1, br.getYesTotal());
        assertEquals(2, br.getNoTotal());
        assertEquals(3, br.getTotal());
        assertEquals(qova.enums.ResponseType.BINARY_ANSWER, br.getType());
        assertEquals(0, br.getSurveyPosition());
        assertEquals(surveyResponse, br.getSurveyResponse());
    }

    @Test
    public void TextResponseConstructorTest(){

        SurveyResponse surveyResponse = Mockito.mock(SurveyResponse.class);
        
        var question = "Was the lecture informative";
        
        TextResponse tr = new TextResponse(surveyResponse, question, 1);
        
        assertEquals(question, tr.getQuestion());
        assertEquals(qova.enums.ResponseType.TEXT_RESPONSE, tr.getType());
        assertEquals(1, tr.getSurveyPosition());
        assertEquals(surveyResponse, tr.getSurveyResponse());
    }

    @Test
    public void SingleChoiceConstructorTest() throws Exception {

        SurveyResponse surveyResponse = Mockito.mock(SurveyResponse.class);
        
        String question = "Rate the lecutre from 1 to 5";

        ArrayList<String> scOptions = new ArrayList<String>();
        scOptions.add("1");
        scOptions.add("2");
        scOptions.add("3");
        scOptions.add("4");
        scOptions.add("5");
        SingleChoiceResponse scr = new SingleChoiceResponse(surveyResponse, question, 2, scOptions);


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
        assertEquals(qova.enums.ResponseType.SINGLE_CHOICE, scr.getType());
        assertEquals(2, scr.getSurveyPosition());
        assertEquals(surveyResponse, scr.getSurveyResponse());
    }


    @Test
    public void MultipleChoiceConstructorTest() throws Exception {

        SurveyResponse surveyResponse = Mockito.mock(SurveyResponse.class);
        
        String question = "What was good about the lecture (multiple options can be selected)";

        ArrayList<String> mcOptions = new ArrayList<String>();
        mcOptions.add("It was informative");
        mcOptions.add("It was interesting");
        mcOptions.add("I learned something new");
        mcOptions.add("I enjoyed attending the lecture");
        mcOptions.add("I would recommend the lecture to others");
        MultipleChoiceResponse mcr = new MultipleChoiceResponse(surveyResponse, question, 0, mcOptions);

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
        assertEquals(mcOptions, mcr.getMultipleChoiceOptions());
        assertEquals(totals, mcr.getMultipleChoiceAnswers());
        assertEquals(5, mcr.getNumberOfOptions());
        assertEquals(qova.enums.ResponseType.MULTIPLE_CHOICE, mcr.getType());
        assertEquals(0, mcr.getSurveyPosition());
        assertEquals(surveyResponse, mcr.getSurveyResponse());
    }

}


