package qova.responses;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Objects;

import org.jfree.chart.JFreeChart;
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


    // public void GeneratePDF() throws IOException{

    //     try{
    //         String DEST = "src/main/resources/test.pdf";

    //         File file = new File(DEST);
    //         file.getParentFile().mkdirs();

    //         new PDFGenerator().createPdf(DEST);
    //     }
    //     catch(IOException e){
    //         e.printStackTrace();
    //     }
        
    // }
















































    //Test Method, remove in final build
    public void TestCreateResponses(Course course){
        for(int i = 0; i < 60; i++){
            var courseType = CourseType.LECTURE;
            var position = i % 3;

            if(position % 3 == 0){
            var responseType = ResponseType.BINARY_ANSWER;
            

            if(i < 20){
                var binaryAnswer = false;
                responses.save(new Response(LocalDateTime.now(), course, courseType, position, responseType, binaryAnswer));
            }
            else{
                var binaryAnswer = true;
                responses.save(new Response(LocalDateTime.now(), course, courseType, position, responseType, binaryAnswer));
                
            }

            }

            else if(position % 3 == 1){
                var responseType = ResponseType.MULTIPLE_CHOICE;
                var responsePossiblilites = 4;

                var answer5 = false;
                var answer6 = false;
                var answer7 = false;
                var answer8 = false;
                var answer9 = false;
                var answer10 = false;

                if(i<15){
                    var answer1 = true;
                    var answer2 = false;
                    var answer3 = false;
                    var answer4 = false;
                    responses.save(new Response(LocalDateTime.now(), course, courseType, position, responsePossiblilites, responseType, answer1, answer2, answer3, answer4, answer5, answer6, answer7, answer8, answer9, answer10));
                }
                else if(i < 20){
                    var answer1 = false;
                    var answer2 = true;
                    var answer3 = false;
                    var answer4 = false;
                    responses.save(new Response(LocalDateTime.now(), course, courseType, position, responsePossiblilites, responseType, answer1, answer2, answer3, answer4, answer5, answer6, answer7, answer8, answer9, answer10));
                }
                else if(i < 25){
                    var answer1 = false;
                    var answer2 = false;
                    var answer3 = true;
                    var answer4 = false;
                    responses.save(new Response(LocalDateTime.now(), course, courseType, position, responsePossiblilites, responseType, answer1, answer2, answer3, answer4, answer5, answer6, answer7, answer8, answer9, answer10));
                }
                else{
                    var answer1 = false;
                    var answer2 = false;
                    var answer3 = false;
                    var answer4 = true;
                    responses.save(new Response(LocalDateTime.now(), course, courseType, position, responsePossiblilites, responseType, answer1, answer2, answer3, answer4, answer5, answer6, answer7, answer8, answer9, answer10));
                }
                
                
                

            }

            else{
                var responseType = ResponseType.TEXT_RESPONSE;
                var textResponse = "some extended text response for testing purposes";

                responses.save(new Response(LocalDateTime.now(), course, courseType, position, responseType, textResponse));
            }
        }
        
        
    }
    
}