package com.molkky.molkky.repository;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.rounds.Pool;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface MatchRepository extends JpaRepository<Match, String>, JpaSpecificationExecutor<Match> {
    Match findById(Integer id);
    Match findByPool(Pool pool);
}
