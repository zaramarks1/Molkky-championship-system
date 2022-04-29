package com.molkky.molkky.entity.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.SimpleGameRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class SimpleGameEntityTest {
    @Autowired
    private SimpleGameRepository simpleGameRepository;
    @Autowired
    private SetRepository setRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertSimpleGame() {
        Set set = new Set();
        Set set2 = new Set();
        List<Set> sets = Arrays.asList(set, set2);

        SimpleGame simpleGame = new SimpleGame();
        set.setRound(simpleGame);
        set2.setRound(simpleGame);
        simpleGame.setSets(sets);

        Tournament tournament = tournamentRepository.save(new Tournament(
                "tournament_name",
                "location",
                new Date(),
                new Date(),
                1,
                2,
                true,
                2,
                3
        ));

        simpleGame.setTournament(tournament);
        simpleGame = simpleGameRepository.save(simpleGame);
        System.out.println(simpleGame.getSets());
        Assertions.assertNotNull(simpleGame.getId());
        Set recupSet = setRepository.findById(set.getId());
        Assertions.assertEquals(recupSet.getRound().getId(), simpleGame.getId());

        simpleGame = simpleGameRepository.findById(simpleGame.getId());
        Assertions.assertEquals(2, simpleGame.getSets().size());
    }
}
