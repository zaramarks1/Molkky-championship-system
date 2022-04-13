package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Tournament;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public interface TournamentRepository extends JpaRepository<Tournament, String>, JpaSpecificationExecutor<Tournament> {
    Tournament findById(Integer id);
    Tournament findByName(String tournamentName);
    List<Tournament> findByIsVisibleAndStatus(boolean isVisible, String status);

}
