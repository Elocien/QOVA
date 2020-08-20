package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.CourseInstance;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseInstanceRepository extends CrudRepository <CourseInstance, Long> {
    
}