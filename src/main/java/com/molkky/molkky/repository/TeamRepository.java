package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Team;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TeamRepository extends JpaRepository<Team, String>, JpaSpecificationExecutor<Team> {
    Team findById(Integer id);
    Team findByName(String teamName);
    @Query(value="SELECT t.name FROM Team t")
    List<String> findTeamsName();
}
