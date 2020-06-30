package qova.responseTypes;

import qova.course.Course;
import qova.course.CourseType;


import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface UserResponseRepository extends CrudRepository <UserResponse, Long> {
    
    public Iterable<UserResponse> findByCourseAndCourseTypeAndClassNo(Course course, CourseType type, Integer classNo);

    public Iterable<UserResponse> findByCourseAndCourseType(Course course, CourseType type);

}
