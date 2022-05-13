package com.molkky.molkky.entity.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.SimpleGame;
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
        Match Match = new Match();
        Match match2 = new Match();
        List<Match> matchs = Arrays.asList(Match, match2);

        SimpleGame simpleGame = new SimpleGame();
        Match.setRound(simpleGame);
        match2.setRound(simpleGame);
        simpleGame.setMatches(matchs);

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
        System.out.println(simpleGame.getMatches());
        Assertions.assertNotNull(simpleGame.getId());
        Match recupMatch = matchRepository.findById(Match.getId());
        Assertions.assertEquals(recupMatch.getRound().getId(), simpleGame.getId());

        simpleGame = simpleGameRepository.findById(simpleGame.getId());
        Assertions.assertEquals(2, simpleGame.getMatches().size());
    }
}
