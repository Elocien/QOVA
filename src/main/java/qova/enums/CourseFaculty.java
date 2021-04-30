package qova.enums;

import org.springframework.ui.Model;
import qova.forms.CourseForm;

/**
 * <p>Enumeration for all faculties of TU Dresden as per: https://tu-dresden.de/tu-dresden/organisation/bereiche-fakultaeten?set_language=en </p>
 * <p>This is given to the model in {@linkplain qova.logic.CourseController#createCourse(Model, CourseForm)}, so the user may select the faculty at which the
 * course is taking place. </p>
 * <p>Also used in the "studentBrowser" html template for ordering courses</p>
 */
public enum CourseFaculty {
    BIOLOGY, 
    CHEMISTRY,
    FOOD_CHEMISTRY, 
    MATHEMATICS, 
    PHYSICS,
    PSYCHOLOGY,
    ARTS_HUMANITIES_SOCIAL_SCIENCE,
    EDUCATION,
    LAW,
    LINGUISTICS,
    LITERATURE,
    CULTURAL_STUDIES,
    ELECTRICAL_AND_COMPUTER_ENGINEERING,
    COMPUTER_SCIENCE,
    MECHANICAL_SCIENCE_AND_ENGINEERING,
    ARCHITECTURE,
    CIVIL_ENGINEERING,
    ENVIRONMENTAL_SCIENCES,
    TRANSPORTATION_AND_TRAFFIC_SCIENCE,
    BUSSINESS_AND_ECONOMICS,
    MEDICINE;

}