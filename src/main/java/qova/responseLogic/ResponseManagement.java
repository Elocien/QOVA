package qova.responseLogic;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;

import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.course.Course;
import qova.course.CourseInstance;
import qova.course.CourseType;
import qova.course.LocalizationOption;
import qova.responseTypes.BinaryResponse;
import qova.responseTypes.MultipleChoiceResponse;
import qova.responseTypes.SingleChoiceResponse;
import qova.responseTypes.TextResponse;
import qova.responseTypes.SurveyResponse;
import qova.responseTypes.SurveyResponseRepository;

@Service
@Transactional
public class ResponseManagement {

    private final SurveyResponseRepository responseRepository;

    @Autowired
    public ResponseManagement(SurveyResponseRepository responses) {
        this.responseRepository = Objects.requireNonNull(responses);
    }


    public CourseType parseType(String stringType){
        
        CourseType type;

        if(stringType.equals("LECTURE")){type = CourseType.LECTURE;}
        else if(stringType.equals("TUTORIAL")){type = CourseType.LECTURE;}
        else if(stringType.equals("SEMINAR")){type = CourseType.LECTURE;}
        else if(stringType.equals("PRACTICAL")){type = CourseType.LECTURE;}
        else type = null;

        return type;
    }
    
    
    //PDF Generation (ENGLISH)
    public byte[] generatePDF_en(Course course, CourseType type, Integer groupNumber, Integer instanceNumber) throws IOException, Exception {

        //retrieve the SurveyResponse object from repository
        Optional<SurveyResponse> rsp = responseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);
        
        
        //Generate PDF 
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(rsp.get(), course.getName(), LocalizationOption.EN);
    }




    //CSV Generation (ENGLISH)
    public byte[] generateCSV_en(Course course, CourseType type, Integer groupNumber, Integer instanceNumber) throws IOException, Exception {

        //retrieve the SurveyResponse object from repository
        Optional<SurveyResponse> rsp = responseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);
        
        //Generate PDF
        CSVGenerator csvGen = new CSVGenerator();
        return csvGen.createCSV(rsp.get(), LocalizationOption.EN);
    }



    public Boolean verifyJsonArray(JSONArray json){
        //check for too large surveys (more than 100 quesitons)
        if(json.length() > 100){
            return false;
        }

        //example string
        // [{"type":"YesNo","question":""},{"type":"MultipleChoice","question":"","answers":["1","2","3","4","5"]},{"type":"DropDown","question":"","answers":["Answer","Answer","Answer"]}]
        return false;
    }




    public void createSurveyResponse(JSONArray json, Course course, String stringType){
        
        //Resolve type and find correct instance of course (lecture, tutorial, etc.)
        CourseType type = parseType(stringType);
        CourseInstance courseInstance = course.getInstance(type);

        //ArrayList with response objects, initialised with the number of questions as size
        ArrayList<Object> responses = new ArrayList<Object>(json.length());

        //parse json to serialise response objects


        
        //for each instance, 
        for(int group = 0; group < courseInstance.getGroupAmount(); group++){
            for(int instance = 0; instance < courseInstance.getInstanceAmount(); instance++){



                responseRepository.save(new SurveyResponse(course, type, instance, group, responses));
            }
        }
    }











    /**
	 * @param id the response id
	 * @return an {@linkplain Optional} of an {@linkplain SurveyResponse}
	 *         with the given id
	 */
	public Optional<SurveyResponse> findById(long id) {
		return responseRepository.findById(id);
	}

    
    /**
     * 
     * @param course    {@linkplain Course} object
     * @param type      {@linkplain CourseType}
     * @param classNo   The number of the corresponding tutorial or seminar (=1 in case CourseType is Lecture) 
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
	public Optional<SurveyResponse> findByCourseAndCourseTypeAndClassNo(Course course, CourseType type, Integer groupNumber, Integer instanceNumber){
		return responseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);
	}
















































    //Test Method, remove in final build
    public void TestCreateResponses(Course course) throws Exception {
        
        var type = CourseType.LECTURE;
        var instanceNumber = 12;
        var groupNumber = 4;

        ArrayList<Object> responses = new ArrayList<Object>();

        

        responseRepository.save(new SurveyResponse(course, type, instanceNumber, groupNumber, responses));
        
    } 



    public byte[] generatePDF_test() throws IOException, Exception {

        //Add responses to arrayList
        Optional<SurveyResponse> rsp = responseRepository.findById(1L);
        
        //Generate PDF 
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(rsp.get(), "test PDF title", LocalizationOption.EN);
    }
}