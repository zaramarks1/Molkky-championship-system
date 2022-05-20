package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Tournament;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PhaseRepository extends JpaRepository<Phase, String>, JpaSpecificationExecutor<Phase> {

    Phase findById(Integer id);
    List<Phase> findByTournament(Tournament tournament);
}
