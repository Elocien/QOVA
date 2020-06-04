package qova.responses;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.course.Course;
import qova.course.CourseType;

@Service
@Transactional
public class ResponseManagement {

    private final ResponseRepository responses;

    @Autowired
    public ResponseManagement(ResponseRepository responses) {
        this.responses = Objects.requireNonNull(responses);
    }

    public void generatePDF() throws IOException, Exception {
        //TODO: Get correct responses
        ArrayList<Response> pdfResponses = new ArrayList<Response>();
        responses.findAll().forEach(pdfResponses::add);
        
        //Generate PDF
        PDFGenerator pdfGen = new PDFGenerator();
        pdfGen.createPdf(pdfResponses);
    }







    /**
	 * @param id the response id
	 * @return an {@linkplain Optional} of an {@linkplain Response}
	 *         with the given id
	 */
	public Optional<Response> findById(long id) {
		return responses.findById(id);
	}
















































    //Test Method, remove in final build
    public void TestCreateResponses(Course course) throws Exception {
        CourseType courseType = CourseType.LECTURE;
        Integer classNo = 1;
        for(int i = 0; i < 60; i++){

            var position = i % 3;

            if(position % 3 == 0){
                var responseType = ResponseType.BINARY_ANSWER;
                var question = "Is this test question good?";

                if(i < 20){
                    var binaryAnswer = false;
                    responses.save(new Response(course, courseType, position, classNo, responseType, question, binaryAnswer));
                }
                else{
                    var binaryAnswer = true;
                    responses.save(new Response(course, courseType, position, classNo, responseType, question, binaryAnswer));
                    
                }
            }

            else if(position % 3 == 1){
                var responseType = ResponseType.DROP_DOWN;
                var responsePossibilites = 4;
                var question = "Is this test question good?";
                ArrayList<String> options = new ArrayList<String>();
                options.add("bad");
                options.add("acceptable");
                options.add("good");
                options.add("perfect");
                

                if(i < 15){
                    responses.save(new Response(course, courseType, position, classNo, responseType, question, responsePossibilites, 0, options));
                }
                else if(i < 20){
                    responses.save(new Response(course, courseType, position, classNo, responseType, question, responsePossibilites, 1, options));
                }
                else if(i < 25){
                    responses.save(new Response(course, courseType, position, classNo, responseType, question, responsePossibilites, 2, options));
                }
                else{
                    responses.save(new Response(course, courseType, position, classNo, responseType, question, responsePossibilites, 3, options));
                }   

            }

            else{
                var responseType = ResponseType.TEXT_RESPONSE;
                var textResponse = "some extended text response for testing purposes";
                var question = "Another test question, respond in text form";

                responses.save(new Response(course, courseType, position, classNo, responseType, question, textResponse));
            }
        }  
    } 
}