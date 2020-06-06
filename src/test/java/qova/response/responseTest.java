package qova.response;

import org.junit.jupiter.api.Test;

import qova.AbstractIntegrationTest;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class responseTest extends AbstractIntegrationTest {
    @Test
	public void courseConstructorTest(){
        var name = "test";
        var lectureExists = true;
        var tutorialExists = false;
        var seminarExists = true;  
        var lectureSurvey = "";
        var tutorialSurvey = "";
        var seminarSurvey = "";
        var classTotalSeminar = 10;
        var classTotalTutorial = 5;
        var semesterOfStudents = 6;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseInstance = LocalDate.of(2020, 10, 4);
        var semesterUI = "SoSe 2020";

        Course crs  = new Course(name, lectureExists, tutorialExists, seminarExists, lectureSurvey, tutorialSurvey, seminarSurvey, classTotalTutorial, classTotalSeminar, semesterOfStudents, faculty, semesterUI, courseInstance);


        assertEquals(name, crs.getName());
        assertEquals(lectureExists, crs.getLectureExists());
        assertEquals(tutorialExists, crs.getTutorialExists());
        assertEquals(seminarExists, crs.getSeminarExists());
        assertEquals(lectureSurvey, crs.getLectureSurvey());
        assertEquals(tutorialSurvey, crs.getTutorialSurvey());
        assertEquals(seminarSurvey, crs.getSeminarSurvey());
        assertEquals(classTotalSeminar, crs.getClassTotalSeminar());
        assertEquals(classTotalTutorial, crs.getClassTotalTutorial());
        assertEquals(semesterOfStudents, crs.getSemesterOfStudents());
        assertEquals(faculty, crs.getFaculty());
        assertEquals(courseInstance, crs.getCourseInstance());
        assertEquals(semesterUI, crs.getSemesterUI());

	}
}


