package com.molkky.molkky.service.pool;

import com.molkky.molkky.domain.Knockout;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.SwissPoolRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;

@Service
public class KnockoutService implements IRoundType<Knockout>{
    private static final Logger logger = LoggerFactory.getLogger(KnockoutService.class);

    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private SwissPoolRepository swissPoolRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private TournamentRepository tournamentRepository;

    public boolean areAllMatchesFinished(Knockout pool) {
        for (Match match : pool.getMatches()) {
            if(Boolean.FALSE.equals(match.getFinished())){
                logger.trace("Match non fini");
                return false;
            }
        }
        logger.trace("Tous les matchs sont finis");
        return true;
    }

    @Override
    @Transactional
    public void generateMatches(Knockout pool, Tournament tournament, List<Team> teams, Integer index){
        List<Match> matches = new ArrayList<>(pool.getMatches());
        for(int i = 0; i < pool.getTeamsRemaining(); i += 2){
            Match match = new Match();
            List<Team> teamsMatch = new ArrayList<>();
            teamsMatch.add(teams.get(i));
            teamsMatch.add(teams.get(i+1));
            match.setTeams(teamsMatch);
            match.setKnockout(tournament.getRounds().get(index).getKnockout());
            match = matchRepository.save(match);
            matches.add(match);
        }
        pool.setMatches(matches);
        roundRepository.save(tournament.getRounds().get(index));
        tournamentRepository.save(tournament);
    }
}
