package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.AbstractResponse;

import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

@Repository
public interface AbstractResponseRepository extends CrudRepository<AbstractResponse, UUID> {

}
