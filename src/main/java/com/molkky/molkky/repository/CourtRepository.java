package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Court;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface CourtRepository extends JpaRepository<Court, String>, JpaSpecificationExecutor<Court> {
    Court findById(Integer id);
}
