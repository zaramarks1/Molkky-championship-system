package com.molkky.molkky.service.pool;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.SwissPool;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class SwissPoolService implements IRoundType<SwissPool>{
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private SwissPoolRepository swissPoolRepository;
    @Autowired
    private MatchRepository matchRepository;

    public boolean areAllMatchesFinished(SwissPool pool) {
        for (Match match : pool.getMatches()) {
            if(Boolean.FALSE.equals(match.getFinished())){
                System.out.println("Match non fini");
                return false;
            }
        }
        System.out.println("Tous les matchs sont finis");
        return true;
    }

    @Override
    public void generateMatches(SwissPool pool, Tournament tournament, List<Team> teams){
        List<Match> matches = new ArrayList<>();
        for(int i = 0; i < tournament.getRounds().get(0).getNbTeams(); i++){
            for(int y = 0; y < tournament.getRounds().get(0).getNbTeams(); y++){
                if(i != y){
                    Match nvMatch = new Match();
                    List<Team> teamsMatch = new ArrayList<>();
                    teamsMatch.add(teams.get(i));
                    teamsMatch.add(teams.get(y));
                    nvMatch.setTeams(teamsMatch);
                    nvMatch.setSwissPool(tournament.getRounds().get(0).getSwissPool());
                    nvMatch = matchRepository.save(nvMatch);
                    matches.add(nvMatch);
                }
            }
        }
        pool.setMatches(matches);
        roundRepository.save(tournament.getRounds().get(0));
    }
}
