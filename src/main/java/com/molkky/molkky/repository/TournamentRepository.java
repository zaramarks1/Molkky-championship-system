package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Tournament;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;


@Repository
public interface TournamentRepository extends JpaRepository<Tournament, String>, JpaSpecificationExecutor<Tournament> {
    Tournament findById(Integer id);
    Tournament findByName(String tournamentName);
    Tournament findByStatus(String status);
}
