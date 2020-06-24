package qova.course;

import org.junit.jupiter.api.Test;

import qova.AbstractIntegrationTest;

import java.time.LocalDate;
import static org.junit.jupiter.api.Assertions.assertEquals;


public class courseTest extends AbstractIntegrationTest {

    @Test
	public void courseConstructorTest() throws Exception {
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


        assertEquals(name, crs.getName());
        assertEquals(true, crs.getLectureExists());
        assertEquals(true, crs.getTutorialExists());
        assertEquals(false, crs.getSeminarExists());
        assertEquals(false, crs.getPracticalExists());
        assertEquals(lecture, crs.getLecture());
        assertEquals(tutorial, crs.getTutorial());
        assertEquals(seminar, crs.getSeminar());
        assertEquals(practical, crs.getPractical());
        assertEquals(semesterOfStudents, crs.getSemesterOfStudents());
        assertEquals(faculty, crs.getFaculty());
        assertEquals(courseDate, crs.getCourseDate());
        assertEquals(semesterString, crs.getSemesterString());

	}

}