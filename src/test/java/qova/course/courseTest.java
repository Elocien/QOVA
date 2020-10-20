package qova.course;

import org.junit.jupiter.api.Test;

import org.mockito.Mockito;
import qova.AbstractIntegrationTest;
import qova.admin.DefaultSurvey;
import qova.enums.CourseFaculty;
import qova.enums.CourseType;
import qova.objects.Course;
import qova.objects.CourseInstance;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class courseTest extends AbstractIntegrationTest {

    @Test
    public void courseConstructorTest() {

        DefaultSurvey defaultSurvey = Mockito.mock(DefaultSurvey.class);

        var name = "Rechnernetze";

        var groupAmount = 10;
        var instanceAmount = 12;
        var active = true;

        List<String> instanceTitles = new ArrayList<>(Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));

        CourseInstance lecture = new CourseInstance(CourseType.LECTURE, groupAmount, instanceAmount, instanceTitles,
                defaultSurvey);

        CourseInstance tutorial = new CourseInstance(CourseType.TUTORIAL, groupAmount, instanceAmount, instanceTitles,
                defaultSurvey);

        CourseInstance seminar = new CourseInstance(CourseType.SEMINAR, defaultSurvey);

        CourseInstance practical = new CourseInstance(CourseType.PRACTICAL, defaultSurvey);


        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        Course crs = new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty,
                semesterString, courseDate);

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