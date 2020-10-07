package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.AbstractResponse;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface AbstractResponseRepository extends CrudRepository<AbstractResponse, Long> {

}
