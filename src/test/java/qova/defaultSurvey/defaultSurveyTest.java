package qova.defaultSurvey;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import qova.AbstractIntegrationTest;
import qova.admin.DefaultSurvey;

public class defaultSurveyTest extends AbstractIntegrationTest {

    @Test
	public void defaultSurveyConstructorTest() {
        
        var survey = "[]";
        var id = 1337L;

        DefaultSurvey defaultSurvey = new DefaultSurvey(id, survey);

        assertEquals(defaultSurvey.getDefaultSurveyJson(), survey);
        assertEquals(defaultSurvey.getId(), id);
	}
    
}
