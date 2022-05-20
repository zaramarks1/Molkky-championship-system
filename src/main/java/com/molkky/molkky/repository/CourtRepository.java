package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Court;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public interface CourtRepository extends JpaRepository<Court, String>, JpaSpecificationExecutor<Court> {
    Court findById(Integer id);
    List<Court> findByAvailable(Boolean available);
}
