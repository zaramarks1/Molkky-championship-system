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
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.data.jdbc.DataJdbcTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Propagation;

import javax.transaction.Transactional;
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
    @Transactional
    void testInsertPool() {
        Match match = new Match();
        Match match2 = new Match();
        List<Match> matches = Arrays.asList(match, match2);

        Pool pool = new Pool();
        match.setPool(pool);
        match2.setPool(pool);
        pool.setMatches(matches);

        pool = poolRepository.save(pool);
        System.out.println(pool.getMatches());
        Assertions.assertNotNull(pool.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getPool().getId(), pool.getId());

        pool = poolRepository.findById(pool.getId());
        Assertions.assertEquals(2, pool.getMatches().size());
    }
}
