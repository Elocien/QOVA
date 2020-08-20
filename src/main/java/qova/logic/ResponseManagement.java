package qova.logic;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.enums.CourseType;
import qova.enums.LocalizationOption;
import qova.enums.ResponseType;
import qova.objects.BinaryResponse;
import qova.objects.Course;
import qova.objects.CourseInstance;
import qova.objects.MultipleChoiceResponse;
import qova.objects.SurveyResponse;
import qova.objects.TextResponse;
import qova.repositories.BinaryResponseRepository;
import qova.repositories.MultipleChoiceResponseRepository;
import qova.repositories.SurveyResponseRepository;
import qova.repositories.TextResponseRepository;



@Service
@Transactional
public class ResponseManagement {

    private final SurveyResponseRepository surveyResponseRepository;
    private final BinaryResponseRepository binaryResponseRepository;
    private final TextResponseRepository textResponseRepository;
    private final MultipleChoiceResponseRepository multipleChoiceResponseRepository;


    @Autowired
    public ResponseManagement(SurveyResponseRepository surveyResponseRepository, BinaryResponseRepository binaryResponseRepository, TextResponseRepository textResponseRepository, MultipleChoiceResponseRepository multipleChoiceResponseRepository) {
        this.surveyResponseRepository = Objects.requireNonNull(surveyResponseRepository);
        this.binaryResponseRepository = Objects.requireNonNull(binaryResponseRepository);
        this.textResponseRepository = Objects.requireNonNull(textResponseRepository);
        this.multipleChoiceResponseRepository = Objects.requireNonNull(multipleChoiceResponseRepository);
    }


    public CourseType parseCourseType(String stringType){
        
        CourseType type;

        if(stringType.equals("LECTURE")){type = CourseType.LECTURE;}
        else if(stringType.equals("TUTORIAL")){type = CourseType.LECTURE;}
        else if(stringType.equals("SEMINAR")){type = CourseType.LECTURE;}
        else if(stringType.equals("PRACTICAL")){type = CourseType.LECTURE;}
        else type = null;

        return type;
    }

    public ResponseType parseResponseType(Object rsp){
        if(rsp instanceof qova.objects.BinaryResponse){
            return ResponseType.BINARY_ANSWER;
        }
        else if(rsp instanceof qova.objects.TextResponse){
            return ResponseType.TEXT_RESPONSE;
        }
        else if(rsp instanceof qova.objects.SingleChoiceResponse){
            return ResponseType.SINGLE_CHOICE;
        }
        else if(rsp instanceof qova.objects.MultipleChoiceResponse){
            return ResponseType.MULTIPLE_CHOICE;
        }
        else{
            return null;
        }
    }
    
    
    //PDF Generation (ENGLISH)
    public byte[] generatePDF_en(Course course, CourseType type, Integer groupNumber, Integer instanceNumber) throws IOException, Exception {

        //retrieve the SurveyResponse object from repository
        Optional<SurveyResponse> rsp = surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);
        
        
        //Generate PDF 
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(rsp.get(), LocalizationOption.EN);
    }




    //CSV Generation (ENGLISH)
    public byte[] generateCSV_en(Course course, CourseType type, Integer groupNumber, Integer instanceNumber) throws IOException, Exception {

        //retrieve the SurveyResponse object from repository
        Optional<SurveyResponse> rsp = surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);
        
        //Generate PDF
        CSVGenerator csvGen = new CSVGenerator();
        return csvGen.createCSV(rsp.get(), LocalizationOption.EN);
    }



    public Boolean verifyJsonArray(JSONArray json){
       
        //check for too large surveys (more than 100 quesitons)
        if(json.length() > 100){
            return false;
        }

        //TODO: Check for other malicious strings

        //example string
        // [{"type":"YesNo","question":""},{"type":"MultipleChoice","question":"","answers":["1","2","3","4","5"]},{"type":"DropDown","question":"","answers":["Answer","Answer","Answer"]}]
        
        return true;
    }




    public void createSurveyResponse(JSONArray json, Course course, String stringType){
        
        //Resolve type and find correct instance of course (lecture, tutorial, etc.)
        CourseType type = parseCourseType(stringType);
        CourseInstance courseInstance = course.getInstance(type);

        //ArrayList with response objects, initialised with the number of questions as size
        List<Object> responses = new ArrayList<>(json.length());

        //parse json to serialise response objects
        

        
        //for each instance, 
        for(int group = 0; group < courseInstance.getGroupAmount(); group++){
            for(int instance = 0; instance < courseInstance.getInstanceAmount(); instance++){



                surveyResponseRepository.save(new SurveyResponse(course, type, instance, group, responses));
            }
        }
    }











    /**
	 * @param id the response id
	 * @return an {@linkplain Optional} of an {@linkplain SurveyResponse}
	 *         with the given id
	 */
	public Optional<SurveyResponse> findById(long id) {
		return surveyResponseRepository.findById(id);
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
		return surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);
	}
















































    //Test Method, remove in build
    public void createTestResponses(Course course) throws Exception {
        
        var type = CourseType.LECTURE;
        var instanceNumber = 12;
        var groupNumber = 4;

        List<Object> responses = new ArrayList<>();

        BinaryResponse bnr = new BinaryResponse("Would you consider recommending the lecture to other students?");
        binaryResponseRepository.save(bnr);
        for(int i = 0; i < 50 ; i++){bnr.incrementYes();}
        for(int i = 0; i < 25 ; i++){bnr.incrementNo();}


        
        
        ArrayList<String> mcOptions = new ArrayList<>();
        mcOptions.add("It was informative");
        mcOptions.add("It was interesting");
        mcOptions.add("I learned something new");
        mcOptions.add("I enjoyed attending the lecture");
        mcOptions.add("I would recommend the lecture to others");
        MultipleChoiceResponse mcr = new MultipleChoiceResponse("What was good about the lecture (multiple options can be selected)", mcOptions);
        multipleChoiceResponseRepository.save(mcr);

        ArrayList<Integer> mcAnswers1 = new ArrayList<>();
        mcAnswers1.add(0);
        mcAnswers1.add(1);
        mcAnswers1.add(3);

        ArrayList<Integer> mcAnswers2 = new ArrayList<>();
        mcAnswers2.add(1);
        mcAnswers2.add(4);


        ArrayList<Integer> mcAnswers3 = new ArrayList<>();
        mcAnswers3.add(2);
        mcAnswers3.add(4);

        for(int i = 0; i < 25 ; i++){mcr.incrementTotals(mcAnswers1);}
        for(int i = 0; i < 15 ; i++){mcr.incrementTotals(mcAnswers2);}
        for(int i = 0; i < 10 ; i++){mcr.incrementTotals(mcAnswers3);}

        


        TextResponse txr = new TextResponse("What is your opinion of the lecture, is it helpful?");
        textResponseRepository.save(txr);
        for(int i = 0; i < 20 ; i++){txr.addTextSubmission("this is a bit of a test");}
        for(int i = 0; i < 10 ; i++){txr.addTextSubmission("this is a larger test to test the test");}
        for(int i = 0; i < 17 ; i++){txr.addTextSubmission("short test");}
        for(int i = 0; i < 3 ; i++){txr.addTextSubmission("this is a very very very very very very very very very very very very very very very very very very very very large test");}


        responses.add(bnr);
        responses.add(mcr);
        responses.add(txr);
        surveyResponseRepository.save(new SurveyResponse(course, type, instanceNumber, groupNumber, responses));
        
    } 



    public byte[] generatePDF_test() throws IOException, Exception {

        //Add responses to arrayList
        Iterable<SurveyResponse> rsp = surveyResponseRepository.findAll();

        List<SurveyResponse> sr = new ArrayList<>();

        for(SurveyResponse r: rsp){
            sr.add(r);
        }
        //Generate PDF 
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(sr.get(0), LocalizationOption.EN);
    }
}