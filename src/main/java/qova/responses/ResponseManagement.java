package qova.responses;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
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

        var courseType = CourseType.LECTURE;

        Response rspBinary = new Response(LocalDateTime.now(), course, courseType, 1, 1, ResponseType.BINARY_ANSWER, 0);
        Response rspText = new Response(LocalDateTime.now(), course, courseType, 1, 2, ResponseType.TEXT_RESPONSE, 0);
        Response rspMultiChoice = new Response(LocalDateTime.now(), course, courseType, 1, 3, ResponseType.MULTIPLE_CHOICE, 4);


        for(int i = 0; i < 60; i++){


            if(i % 3 == 0){

                if(i < 20){
                    rspBinary.addBinaryAnswer(false);
                    
                }
                else{
                    rspBinary.addBinaryAnswer(true);
                }

            }

            else if(i % 3 == 1){

                if(i<15){
                    rspMultiChoice.setMC_or_DD_ResponseTrue(1);
                }
                else if(i < 20){
                    rspMultiChoice.setMC_or_DD_ResponseTrue(2);
                }
                else if(i < 25){
                    rspMultiChoice.setMC_or_DD_ResponseTrue(3);
                }
                else{
                    rspMultiChoice.setMC_or_DD_ResponseTrue(4);
                }
                
                
                

            }

            else{
                if(i<5){
                    rspText.addTextResponse("short test");
                }
                else if(i < 20){
                    rspText.addTextResponse("this is a very very very long test string");
                }
                else if(i < 30){
                    rspText.addTextResponse("this is a medium length test string");
                }
                else{
                    rspText.addTextResponse("this is a slightly longer than medium length test string");
                }
            }
        }

        responses.save(rspBinary);
        responses.save(rspText);
        responses.save(rspMultiChoice);
        
        
    }
    
}