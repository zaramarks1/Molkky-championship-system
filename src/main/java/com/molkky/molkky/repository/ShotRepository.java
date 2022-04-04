package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Shot;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;


@Repository
public interface ShotRepository extends JpaRepository<Shot, String>, JpaSpecificationExecutor<Shot> {
    Shot findById(Integer id);
}
