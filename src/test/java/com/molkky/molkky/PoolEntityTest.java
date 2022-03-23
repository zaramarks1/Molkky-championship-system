package com.molkky.molkky;

import com.molkky.molkky.domain.Court;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Pool;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.PoolRepository;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@SpringBootTest(classes = MolkkyApplication.class)
class PoolEntityTest {
    @Autowired
    private PoolRepository poolRepository;
    @Autowired
    private MatchRepository matchRepository;

    @Test
    void testInsertPool() {
        Match match = matchRepository.save(new Match());
        Match match2 = matchRepository.save(new Match());
        List<Match> matches = Arrays.asList(match, match2);

        Pool pool = new Pool();
        pool.setMatches(matches);
        poolRepository.save(pool);
        Assertions.assertNotNull(pool.getId());
        Match recupMatch = matchRepository.findByPool(pool);
        Assertions.assertEquals(recupMatch.getPool().getId(), pool.getId());
    }
}
