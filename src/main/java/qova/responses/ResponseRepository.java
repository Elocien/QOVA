package qova.responses;

import org.springframework.stereotype.Repository;

import qova.course.Course;

import java.util.ArrayList;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface ResponseRepository extends CrudRepository <Response, Long> {
    
    public ArrayList<Response> findByCourseAndPosition(Course Course, int position);

}
