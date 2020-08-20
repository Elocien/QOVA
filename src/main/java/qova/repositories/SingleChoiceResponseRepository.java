package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.SingleChoiceResponse;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface SingleChoiceResponseRepository extends CrudRepository <SingleChoiceResponse, Long> {

}