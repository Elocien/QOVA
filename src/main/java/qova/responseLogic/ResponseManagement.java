package qova.responseLogic;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.course.Course;
import qova.course.CourseType;
import qova.course.LocalizationOption;
import qova.responseTypes.BinaryResponse;
import qova.responseTypes.MultipleChoiceResponse;
import qova.responseTypes.SingleChoiceResponse;
import qova.responseTypes.TextResponse;
import qova.responseTypes.UserResponse;
import qova.responseTypes.UserResponseRepository;

@Service
@Transactional
public class ResponseManagement {

    private final UserResponseRepository responseRepository;

    @Autowired
    public ResponseManagement(UserResponseRepository responses) {
        this.responseRepository = Objects.requireNonNull(responses);
    }

    
    
    //PDF Generation (ENGLISH)
    public byte[] generatePDF_en(Course course, CourseType courseType, Integer classNo) throws IOException, Exception {

        //Responses used to gen pdg
        ArrayList<UserResponse> pdfResponses = new ArrayList<UserResponse>();

        //Add responses to arrayList
        if(classNo > 0){
            responseRepository.findByCourseAndCourseTypeAndClassNo(course, courseType, classNo).forEach(pdfResponses::add);
        }
        //if classNo is 0, add responses for all classNo's
        else{
            responseRepository.findByCourseAndCourseType(course, courseType).forEach(pdfResponses::add);
        }
        
        
        //Generate PDF 
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(pdfResponses, course.getName(), LocalizationOption.EN);
    }




    //CSV Generation (ENGLISH)
    public byte[] generateCSV_en(Course course, CourseType courseType, Integer classNo) throws IOException, Exception {

        //Responses used to gen pdg
        ArrayList<UserResponse> csvResponses = new ArrayList<UserResponse>();

        //Add responses to arrayList
        if(classNo > 0){
            responseRepository.findByCourseAndCourseTypeAndClassNo(course, courseType, classNo).forEach(csvResponses::add);
        }
        //if classNo is 0, add responses for all classNo's
        else{
            responseRepository.findByCourseAndCourseType(course, courseType).forEach(csvResponses::add);
        }
        
        
        //Generate PDF
        CSVGenerator csvGen = new CSVGenerator();
        return csvGen.createCSV(csvResponses, LocalizationOption.EN);
    }





    /**
	 * @param id the response id
	 * @return an {@linkplain Optional} of an {@linkplain UserResponse}
	 *         with the given id
	 */
	public Optional<UserResponse> findById(long id) {
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
	public Iterable<UserResponse> findByCourseAndCourseTypeAndClassNo(Course course, CourseType type, Integer classNo){
		return responseRepository.findByCourseAndCourseTypeAndClassNo(course, type, classNo);
	}


     /**
     * 
     * @param course    {@linkplain Course} object
     * @param type      {@linkplain CourseType}
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
	public Iterable<UserResponse> findByCourseAndCourseType(Course course, CourseType type){
		return responseRepository.findByCourseAndCourseType(course, type);
	}















































    //Test Method, remove in final build
    public void TestCreateResponses(Course course) throws Exception {
        var type = CourseType.LECTURE;
        var classNo = 1;
        ArrayList<Object> responses1 = new ArrayList<>();
        ArrayList<Object> responses2 = new ArrayList<>();
        ArrayList<Object> responses3 = new ArrayList<>();
        ArrayList<Object> responses4 = new ArrayList<>();
        
        
        
        ArrayList<String> singleChoiceOptions = new ArrayList<String>();
        singleChoiceOptions.add("Not informative");
        singleChoiceOptions.add("A little informative");
        singleChoiceOptions.add("Very informative");

        ArrayList<String> multipleChoiceOptions = new ArrayList<String>();
        multipleChoiceOptions.add("The lecture was informative");
        multipleChoiceOptions.add("The lecture was interesting");
        multipleChoiceOptions.add("I enjoyed attending the lecture");


        //-----------------------------------------------------------------------



        ArrayList<Boolean> singleChoiceAnswer = new ArrayList<Boolean>();
        singleChoiceAnswer.add(false);
        singleChoiceAnswer.add(true);
        singleChoiceAnswer.add(false);

        ArrayList<Boolean> multipleChoiceAnswers = new ArrayList<Boolean>();
        multipleChoiceAnswers.add(false);
        multipleChoiceAnswers.add(true);
        multipleChoiceAnswers.add(true);


        responses1.add(new BinaryResponse("Was the lecture interesting?", true));
        responses1.add(new TextResponse("Do you have any critizisms regarding todays lecture?", "I thought the lecture was paced too slowly"));
        responses1.add(new SingleChoiceResponse("Was the lecture informative?", singleChoiceOptions, singleChoiceAnswer));
        responses1.add(new MultipleChoiceResponse("What was good about the lecture?", multipleChoiceOptions, multipleChoiceAnswers));


        //responses2 
        ArrayList<Boolean> singleChoiceAnswer2 = new ArrayList<Boolean>();
        singleChoiceAnswer.add(true);
        singleChoiceAnswer.add(false);
        singleChoiceAnswer.add(false);

        ArrayList<Boolean> multipleChoiceAnswers2 = new ArrayList<Boolean>();
        multipleChoiceAnswers.add(true);
        multipleChoiceAnswers.add(false);
        multipleChoiceAnswers.add(false);


        responses1.add(new BinaryResponse("Was the lecture interesting?", true));
        responses1.add(new TextResponse("Do you have any critizisms regarding todays lecture?", "It was fine"));
        responses1.add(new SingleChoiceResponse("Was the lecture informative?", singleChoiceOptions, singleChoiceAnswer));
        responses1.add(new MultipleChoiceResponse("What was good about the lecture?", multipleChoiceOptions, multipleChoiceAnswers));



        //responses3
        ArrayList<Boolean> singleChoiceAnswer3 = new ArrayList<Boolean>();
        singleChoiceAnswer.add(false);
        singleChoiceAnswer.add(false);
        singleChoiceAnswer.add(true);

        ArrayList<Boolean> multipleChoiceAnswers3 = new ArrayList<Boolean>();
        multipleChoiceAnswers.add(false);
        multipleChoiceAnswers.add(false);
        multipleChoiceAnswers.add(false);


        responses1.add(new BinaryResponse("Was the lecture interesting?", true));
        responses1.add(new TextResponse("Do you have any critizisms regarding todays lecture?", "It was quite boring"));
        responses1.add(new SingleChoiceResponse("Was the lecture informative?", singleChoiceOptions, singleChoiceAnswer));
        responses1.add(new MultipleChoiceResponse("What was good about the lecture?", multipleChoiceOptions, multipleChoiceAnswers));



        //responses4
        ArrayList<Boolean> singleChoiceAnswer4 = new ArrayList<Boolean>();
        singleChoiceAnswer.add(false);
        singleChoiceAnswer.add(true);
        singleChoiceAnswer.add(false);

        ArrayList<Boolean> multipleChoiceAnswers4 = new ArrayList<Boolean>();
        multipleChoiceAnswers.add(true);
        multipleChoiceAnswers.add(false);
        multipleChoiceAnswers.add(true);


        responses1.add(new BinaryResponse("Was the lecture interesting?", false));
        responses1.add(new TextResponse("Do you have any critizisms regarding todays lecture?", "I don't have any direct critizisms, but it would be great if Prof. Anderson would increase the volume level of the mic for next time"));
        responses1.add(new SingleChoiceResponse("Was the lecture informative?", singleChoiceOptions, singleChoiceAnswer));
        responses1.add(new MultipleChoiceResponse("What was good about the lecture?", multipleChoiceOptions, multipleChoiceAnswers));

        
        for(int i = 0; i < 15; i++){
            responseRepository.save(new UserResponse(course, type, classNo, responses1));
        }
        for(int i = 0; i < 10; i++){
            responseRepository.save(new UserResponse(course, type, classNo, responses2));
        }
        for(int i = 0; i < 12; i++){
            responseRepository.save(new UserResponse(course, type, classNo, responses3));
        }
        for(int i = 0; i < 8; i++){
            responseRepository.save(new UserResponse(course, type, classNo, responses4));
        }



        
    } 



    public byte[] generatePDF_test() throws IOException, Exception {

        //Responses used to gen pdg
        ArrayList<UserResponse> pdfResponses = new ArrayList<UserResponse>();

        //Add responses to arrayList
        responseRepository.findAll().forEach(pdfResponses::add);
        
        //Generate PDF 
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(pdfResponses, "test PDF title");
    }
}