package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Team;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface TeamRepository extends JpaRepository<Team, String>, JpaSpecificationExecutor<Team> {
    Team findById(Integer id);
}
