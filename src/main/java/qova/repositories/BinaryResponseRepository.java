package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.BinaryResponse;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface BinaryResponseRepository extends CrudRepository <BinaryResponse, Long> {

}