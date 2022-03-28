package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.SimpleGame;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.MatchRepository;
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
    private MatchRepository matchRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertSimpleGame() {
        Match match = new Match();
        Match match2 = new Match();
        List<Match> matches = Arrays.asList(match, match2);

        SimpleGame simpleGame = new SimpleGame(2);
        match.setSimpleGame(simpleGame);
        match2.setSimpleGame(simpleGame);
        simpleGame.setMatches(matches);

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
        Round round = new Round("simplegame", 2);
        round.setTournament(tournament);
        roundRepository.save(round);
        simpleGame.setRound(round);

        simpleGame = simpleGameRepository.save(simpleGame);
        System.out.println(simpleGame.getMatches());
        Assertions.assertNotNull(simpleGame.getId());
        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertEquals(recupMatch.getSimpleGame().getId(), simpleGame.getId());

        simpleGame = simpleGameRepository.findById(simpleGame.getId());
        Assertions.assertEquals(2, simpleGame.getMatches().size());
    }
}
