package qova.course;

import org.junit.jupiter.api.Test;

import qova.AbstractIntegrationTest;

import java.time.LocalDate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.springframework.beans.factory.annotation.Autowired;


public class courseManagementTest extends AbstractIntegrationTest {
    
    @Autowired
    CourseManagement courseManagement;


    @Test
	public void dateParsingTest(){
		//WiSe
		LocalDate semWiSe = courseManagement.parseSemesterString("WiSe 2019/2020");
		assertEquals(LocalDate.of(2019, 10, 1), semWiSe);

		//SoSe
		LocalDate semSoSe = courseManagement.parseSemesterString("SoSe 2020");
		assertEquals(LocalDate.of(2020, 4, 1), semSoSe);

	}

}