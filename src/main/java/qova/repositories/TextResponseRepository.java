package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.TextResponse;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface TextResponseRepository extends CrudRepository <TextResponse, Long> {

}