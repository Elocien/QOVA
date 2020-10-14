package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.CourseInstance;

import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

@Repository
public interface CourseInstanceRepository extends CrudRepository <CourseInstance, UUID> {
    
}