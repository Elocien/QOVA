package qova.logic;

import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javassist.bytecode.ByteArray;
import qova.admin.DefaultSurvey;
import qova.enums.CourseFaculty;
import qova.enums.CourseType;
import qova.enums.LocalizationOption;
import qova.enums.ResponseType;
import qova.objects.BinaryResponse;
import qova.objects.Course;
import qova.objects.CourseInstance;
import qova.objects.MultipleChoiceResponse;
import qova.objects.SingleChoiceResponse;
import qova.objects.SurveyResponse;
import qova.objects.TextResponse;
import qova.repositories.BinaryResponseRepository;
import qova.repositories.MultipleChoiceResponseRepository;
import qova.repositories.SingleChoiceResponseRepository;
import qova.repositories.SurveyResponseRepository;
import qova.repositories.TextResponseRepository;



@Service
@Transactional
public class ResponseManagement {

    private final SurveyResponseRepository surveyResponseRepository;

    private final BinaryResponseRepository binaryResponseRepository;

    private final TextResponseRepository textResponseRepository;
    
    private final SingleChoiceResponseRepository singleChoiceResponseRepository;
    
    private final MultipleChoiceResponseRepository multipleChoiceResponseRepository;


    @Autowired
    public ResponseManagement(SurveyResponseRepository surveyResponseRepository, BinaryResponseRepository binaryResponseRepository, TextResponseRepository textResponseRepository,
            SingleChoiceResponseRepository singleChoiceResponseRepository, MultipleChoiceResponseRepository multipleChoiceResponseRepository) {
        this.surveyResponseRepository = Objects.requireNonNull(surveyResponseRepository);
        this.binaryResponseRepository = Objects.requireNonNull(binaryResponseRepository);
        this.textResponseRepository = Objects.requireNonNull(textResponseRepository);
        this.singleChoiceResponseRepository = Objects.requireNonNull(singleChoiceResponseRepository);
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
    public byte[] generatePDF_en(Course course, CourseType type, Integer groupNumber, Integer instanceNumber) throws Exception {

        //retrieve the SurveyResponse object from repository
        Optional<SurveyResponse> rsp = surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);
        
        
        //Generate PDF 
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(rsp.get(), findResponsesBySurveyResponse(rsp.get()),LocalizationOption.EN);
    }





    /**
     * This method retrieves all of the surveyResponses 
     * 
     * @param course
     * @param type
     * @param groupNumber
     * @param instanceNumber
     * @return
     * @throws Exception
     */
    public byte[] generateCSV_en(Course course, CourseType type, String groupNumber, String instanceNumber) throws Exception {
        
        //Initalise List of SurveyResponses to be passed to the CSV generator
        List<SurveyResponse> listOfSurveyResponses = new ArrayList<>();
    

        if(groupNumber.equals("all") && instanceNumber.equals("all") ){
            
            findSurveyResponseByCourseAndCourseType(course, type).forEach(listOfSurveyResponses::add);

        }
        else if(groupNumber.equals("all")){
            findSurveyResponseByCourseAndCourseTypeAndInstanceNumber(course, type, Integer.parseInt(instanceNumber)).forEach(listOfSurveyResponses::add);
            System.out.println(listOfSurveyResponses);
        }
        else if(instanceNumber.equals("all") ){
            findSurveyResponseByCourseAndCourseTypeAndGroupNumber(course, type, Integer.parseInt(groupNumber)).forEach(listOfSurveyResponses::add);
        }
        else{
            Optional<SurveyResponse> s = findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, Integer.parseInt(groupNumber), Integer.parseInt(instanceNumber));
            if(s.isPresent()){
                listOfSurveyResponses.add(s.get());
            }
        }
        

        //Create List containing lists of surveyObjects (BinaryResponse, TextResponse, etc.)
        List<List<Object>> listOfResponseObjects = new ArrayList<>();

        for(SurveyResponse r : listOfSurveyResponses){
            listOfResponseObjects.add(findResponsesBySurveyResponse(r));
        }

        if(listOfResponseObjects.isEmpty()){
            return new byte[0];
        }

        //Generate PDF
        CSVGenerator csvGen = new CSVGenerator();
        return csvGen.createCSV(course, listOfResponseObjects, LocalizationOption.EN);
    }





    /**
     * Verifies that the length of the submitted JSONArray does not exceed 100 elements, as the number of questions per survey is limited to 100;
     * 
     * @param json {@link org.json.JSONArray}
     * @return A boolean flag, which is handled in the controller
     */
    public Boolean verifyJsonArray(JSONArray jsonArray){
       
        //check for too large surveys (more than 100 quesitons)
        if(jsonArray.length() > 100){
            return false;
        }

        
        for (int i = 0; i < jsonArray.length(); i++){
            JSONObject question = jsonArray.getJSONObject(i);
            if(question.getString("question").length() > 1024){
                return false;
            }
        }


        //TODO: Check for other malicious strings

        //example string
        // [{"type":"YesNo","question":""},{"type":"MultipleChoice","question":"","answers":["1","2","3","4","5"]},{"type":"DropDown","question":"","answers":["Answer","Answer","Answer"]}]
        
        return true;
    }



    /** 
     * Used in the PostMapping of the SurveyEditor. When a survey is created, this method is used to serialise the {@linkplain SurveyResponse}s needed to persist students answers 
     * 
     * @param jsonArray json.org.JSONArray containing the JSON string representing the created survey
     * @param course {@linkplain Course}  
     * @param stringType {@linkplain CourseType} as a String, passed from the model
     */
    public void createSurveyResponse(JSONArray jsonArray, Course course, String stringType){
        
        //Resolve type and find correct instance of course (lecture, tutorial, etc.)
        CourseType type = parseCourseType(stringType);
        CourseInstance courseInstance = course.getInstance(type);


        //for each instance and group, create a SurveyResponse and persist it
        for(int group = 1; group <= courseInstance.getGroupAmount(); group++){
            for(int instance = 1; instance <= courseInstance.getInstanceAmount(); instance++){

                SurveyResponse response = new SurveyResponse(course, type, instance, group);
                
                 //parse json to serialise response objects
                for (int surveyPosition = 0; surveyPosition < jsonArray.length(); surveyPosition++){
                    JSONObject question = jsonArray.getJSONObject(surveyPosition);

                    switch (question.getString("type")) {
                        case "YesNo":
                            binaryResponseRepository.save(new BinaryResponse(response, question.getString("question"), surveyPosition));
                            
                            break;

                        case "FreeText" :
                            textResponseRepository.save(new TextResponse(response, question.getString("question"), surveyPosition));
                            break;

                        case "MultipleChoice" :
                            //Array of all possibilities
                            JSONArray multipleChoiceAnswerOptions = question.getJSONArray("answers");
                            
                            //Array of all possibilieties, passed to the constructor of the MultipleChoiceResponse
                            ArrayList<String> multipleChoiceOptions = new ArrayList<>(multipleChoiceAnswerOptions.length());

                            for(int j = 0; j < multipleChoiceAnswerOptions.length(); j++){
                                multipleChoiceOptions.add(multipleChoiceAnswerOptions.getString(j));
                            }

                            multipleChoiceResponseRepository.save(new MultipleChoiceResponse(response, question.getString("question"), surveyPosition, multipleChoiceOptions));
                            break;

                        case "SingleChoice" :
                            //Array of all possibilities
                            JSONArray singleChoiceAnswerOptions = question.getJSONArray("answers");
                            
                            //Array of all possibilieties, passed to the constructor of the MultipleChoiceResponse
                            ArrayList<String> singleChoiceOptions = new ArrayList<>(singleChoiceAnswerOptions.length());

                            for(int j = 0; j < singleChoiceAnswerOptions.length(); j++){
                                singleChoiceOptions.add(singleChoiceAnswerOptions.getString(j));
                            }

                            singleChoiceResponseRepository.save(new SingleChoiceResponse(response, question.getString("question"), surveyPosition, singleChoiceOptions));
                            break;

                        default:
                            break;
                    }
                }

                //save the new response
                surveyResponseRepository.save(response);
            }
        }
    }











    /**
	 * @param id the response id
	 * @return an {@linkplain Optional} of an {@linkplain SurveyResponse}
	 *         with the given id
	 */
	public Optional<SurveyResponse> findSurveyResponseById(long id) {
		return surveyResponseRepository.findById(id);
	}

    
    /**
     * 
     * @param course    {@linkplain Course} object
     * @param type      {@linkplain CourseType}
     * @param groupNumber The number of the corresponding tutorial or seminar (=1 in case CourseType is Lecture) 
     * @param instanceNumber The number corresponding to the instance of the Course. 
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
	public Optional<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(Course course, CourseType type, Integer groupNumber, Integer instanceNumber){
		return surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);
	}


    /**
     * 
     * @param course    {@linkplain Course} object
     * @param type      {@linkplain CourseType}
     * @param groupNumber The number of the corresponding tutorial or seminar (=1 in case CourseType is Lecture) 
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
	public Iterable<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndGroupNumber(Course course, CourseType type, Integer groupNumber){
		return surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumber(course, type, groupNumber);
	}


    /**
     * 
     * @param course    {@linkplain Course} object
     * @param type      {@linkplain CourseType}
     * @param instanceNumber The number corresponding to the instance of the Course. 
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
	public Iterable<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndInstanceNumber(Course course, CourseType type, Integer instanceNumber){
		return surveyResponseRepository.findByCourseAndCourseTypeAndInstanceNumber(course, type, instanceNumber);
	}


    /**
     * 
     * @param course    {@linkplain Course} object
     * @param type      {@linkplain CourseType}
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
	public Iterable<SurveyResponse> findSurveyResponseByCourseAndCourseType(Course course, CourseType type){
		return surveyResponseRepository.findByCourseAndCourseType(course, type);
    }
    
    public List<Object> findResponsesBySurveyResponse(SurveyResponse surveyResponse){

        List<Object> listOfResponses = new ArrayList<>();
        
        int surveylength = new JSONArray(surveyResponse.getCourse().getInstance(surveyResponse.getCourseType()).getSurvey()).length();

        for(int i = 0; i < surveylength; i++){
            listOfResponses.add("");
        }

        for(BinaryResponse br : binaryResponseRepository.findBySurveyResponse(surveyResponse)){
            listOfResponses.set(br.getSurveyPosition(), br);
        }

        for(TextResponse tr : textResponseRepository.findBySurveyResponse(surveyResponse)){
            listOfResponses.set(tr.getSurveyPosition(), tr);
        }

        for(SingleChoiceResponse scr : singleChoiceResponseRepository.findBySurveyResponse(surveyResponse)){
            listOfResponses.set(scr.getSurveyPosition(), scr);
        }

        for(MultipleChoiceResponse mcr : multipleChoiceResponseRepository.findBySurveyResponse(surveyResponse)){
            listOfResponses.set(mcr.getSurveyPosition(), mcr);
        }     
        
        return listOfResponses;
    }
















































    //Test Method, remove in build
    public void createTestResponses(Course course) {

        var type = CourseType.TUTORIAL;
        var instance = 1;
        var group = 1;

        SurveyResponse response = new SurveyResponse(course, type, instance, group);
        surveyResponseRepository.save(response);

        BinaryResponse bnr = new BinaryResponse(response, "Would you consider recommending the lecture to other students?", 0);
        binaryResponseRepository.save(bnr);
        for(int i = 0; i < 35 ; i++){bnr.incrementYes();}
        for(int i = 0; i < 15 ; i++){bnr.incrementNo();}



        ArrayList<String> mcOptions = new ArrayList<>();
        mcOptions.add("1");
        mcOptions.add("2");
        mcOptions.add("3");
        mcOptions.add("4");
        mcOptions.add("5");


        MultipleChoiceResponse mcr = new MultipleChoiceResponse(response, "From 1 to 5, what would you rate the lecture?", 1, mcOptions);
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

        TextResponse txr = new TextResponse(response, "What is your opinion of the lecture, is it helpful?", 2);
        textResponseRepository.save(txr);
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
    } 


    


    public byte[] generatePDF_test() throws IOException, Exception {

        //Add responses to arrayList
        Iterable<SurveyResponse> rsp = surveyResponseRepository.findAll();

        //Generate PDF 
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(rsp.iterator().next(), findResponsesBySurveyResponse(rsp.iterator().next()), LocalizationOption.EN);
    }
}