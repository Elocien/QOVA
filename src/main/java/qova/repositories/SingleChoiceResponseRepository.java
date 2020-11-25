package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.SingleChoiceResponse;

import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

@Repository
public interface SingleChoiceResponseRepository extends CrudRepository<SingleChoiceResponse, UUID> {

}