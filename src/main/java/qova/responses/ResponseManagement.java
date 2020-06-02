package qova.responses;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;

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


    /**
	 * @param id the response id
	 * @return an {@linkplain Optional} of an {@linkplain Response}
	 *         with the given id
	 */
	public Optional<Response> findById(long id) {
		return responses.findById(id);
	}
















































    //Test Method, remove in final build
    public void TestCreateResponses(Course course){
        CourseType courseType = CourseType.LECTURE;
        Integer classNo = 1;
        for(int i = 0; i < 60; i++){

            var position = i % 3;

            if(position % 3 == 0){
            var responseType = ResponseType.BINARY_ANSWER;

                if(i < 20){
                    var binaryAnswer = false;
                    responses.save(new Response(course, courseType, position, classNo, responseType, binaryAnswer));
                }
                else{
                    var binaryAnswer = true;
                    responses.save(new Response(course, courseType, position, classNo, responseType, binaryAnswer));
                    
                }
            }

            else if(position % 3 == 1){
                var responseType = ResponseType.MULTIPLE_CHOICE;
                var responsePossibilites = 4;

                if(i < 15){
                    responses.save(new Response(course, courseType, position, classNo, responseType, responsePossibilites, 0));
                }
                else if(i < 20){
                    responses.save(new Response(course, courseType, position, classNo, responseType, responsePossibilites, 1));
                }
                else if(i < 25){
                    responses.save(new Response(course, courseType, position, classNo, responseType, responsePossibilites, 2));
                }
                else{
                    responses.save(new Response(course, courseType, position, classNo, responseType, responsePossibilites, 3));
                }   

            }

            else{
                var responseType = ResponseType.TEXT_RESPONSE;
                var textResponse = "some extended text response for testing purposes";

                responses.save(new Response(course, courseType, position, classNo, responseType, textResponse));
            }
        }  
    } 
}