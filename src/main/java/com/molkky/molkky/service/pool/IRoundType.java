package com.molkky.molkky.service.pool;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;

import java.util.List;

public interface IRoundType<T> {
    boolean areAllMatchesFinished(T pool);

    void generateMatches(T pool, Tournament tournament, List<Team> teams, Integer index);
}
