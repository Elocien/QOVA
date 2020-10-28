package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.MultipleChoiceResponse;

import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

@Repository
public interface MultipleChoiceResponseRepository extends CrudRepository<MultipleChoiceResponse, UUID> {

}