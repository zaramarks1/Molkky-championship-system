package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Tournament;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Transactional
@Repository
public interface SearchRepository extends JpaRepository<Tournament, String>, JpaSpecificationExecutor<Tournament> {
    @Query(value = "SELECT * FROM tournament u WHERE u.name LIKE %?1%",
            nativeQuery = true)
    List<Tournament> searchTournamentsByName(String searchTerm);
}
