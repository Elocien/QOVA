package qova.course;

import org.springframework.stereotype.Repository;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseInstanceRepository extends CrudRepository <CourseInstance, Long> {
    
}